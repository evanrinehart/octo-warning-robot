{-# LANGUAGE ScopedTypeVariables #-}
module Object where

import Control.Concurrent
import Control.Concurrent.Chan
import Data.Maybe
import Data.Map
import Control.Monad.Fix
import Control.Monad
import qualified Data.Map as M
import Control.Exception

import Value
import Expr
import Error
import Trap

data Object = Object {
  name     :: Value,
  response :: MVar (Either Value Value),
  fifo     :: Chan (Message Value Value),
  storage  :: MVar (Map Value Value),
  tid      :: MVar ThreadId
}

data ObjectCondition =
  ObjectNormal |
  ObjectError |
  ObjectHalt
    deriving (Show)

data Message a b =
  Message (a, Maybe (MVar (Either b a))) |
  EndOfMessages

objectLookup :: Object -> Value -> IO (Maybe Value)
objectLookup o v = withMVar (storage o) (return . M.lookup v)

objectStore :: Object -> Value -> Value -> IO ()
objectStore o field v = modifyMVar_ (storage o) (return . M.insert field v)

newEmptyObject :: Value -> IO Object
newEmptyObject newName = do
  mv1 <- newEmptyMVar
  mv2 <- newMVar M.empty
  mv3 <- newEmptyMVar
  chan <- newChan
  return $ Object {
    name = newName,
    response = mv1,
    fifo = chan,
    storage = mv2,
    tid = mv3
  }

objectLoop :: Object -> (Value -> IO Value) -> (Value -> IO ()) -> IO ()
objectLoop o react cleanUp = do
  (arg, out) <- fix $ \process -> do
    y <- readChan (fifo o)
    case y of
      EndOfMessages -> process -- shouldnt happen
      Just message  -> return message
  let
    loop = objectLoop o react
    respondNormally result = case out of
      Just port -> do
        putMVar (Right result) port
        loop
      Nothing -> loop
    handleError err = case out of
      Just port ->
        putMVar (Left err) port
        loop
      Nothing -> loop
    halt v = case out of
      Just port -> do
        putMVar (Left v) port
        cleanUp v
      Nothing -> do
        cleanUp v

  (react arg >>= respondNormally) `catches`
    [Handler (\(ex :: Error) -> handleError (fromError ex)),
     Handler (
       \(ex :: Trap Value) -> case ex of
          HaltTrap v  -> halt v
          ErrorTrap v -> handleError v
          AsyncTrap v -> cleanUp v
    )]

