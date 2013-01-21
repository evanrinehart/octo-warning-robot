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
  response :: MVar (React a),
  fifo     :: Chan (Message Value),
  storage  :: MVar (Map Value Value),
  tid      :: MVar ThreadId
}

data React a =
  Normal a |
  SoftCrash a |
  HardCrash a |
  SelfDestruct a |
  Terminated a

data Message a =
  Message (a, Maybe (MVar (React a))) |
  EndOfMessages

-- NEXT abstract the request/response shit into its own data structure
-- you need a Chan of messages with return MVars
-- and you need a return mvar.
-- methods include getting the next message (ignoring end of messages)
-- getting all messages until END OF MESSAGES
-- writing a result to a response port (ignoring Nothing)

nextMessage :: Chan (Message a b) -> IO (a, Maybe (MVar (Either b a)))
nextMessage ch = do
  y <- readChan ch
  case y of
    EndOfMessages -> nextMessage ch
    Message x -> return x

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

whenJust :: Maybe a -> (a -> IO ()) -> IO ()
whenJust Nothing _ = return ()
whenJust (Just x) f = f x

objectLoop ::
  Global ->
  Object ->
  (Value -> IO (React Value)) ->
  IO ()
objectLoop g obj react = do
  (arg, mport) <- nextMessage (fifo obj)
  let
    loop = objectLoop g obj react
    out v = case mport of
      Nothing -> return ()
      Just port -> putMVar port v
  y <- fmap Normal (react arg) `catches` objectCatches
  case y of
    Normal v -> out (Right v) >> loop
    SoftCrash v -> out (Left v) >> loop
    HardCrash v -> out (Left v) >> objectEnd g obj v
    SelfDestruct v -> out (
    

objectStart :: Global -> Value -> (Value -> IO Value) -> IO ()
objectStart g name react = modifyMVar_ g $ \gmap -> do
  obj <- newEmptyObject name
  gmap' <- if member name gmap
    then throw ObjectExistsError
    else return (insert name obj gmap)
  forkIO $ objectLoop g obj react `finally` objectEnd g obj
  return gmap'

globalWrite g name obj

objectEnd :: Global -> Object -> Value -> IO ()
objectEnd

objectCatches :: [Handler Value]
objectCatches = [
    [Handler (\(ex :: Error) -> HardCrash (fromError ex)),
     Handler (
       \(ex :: Trap Value) -> case ex of
          HaltTrap v  -> SelfDestruct v
          ErrorTrap v -> SoftCrash v
          AsyncTrap v -> Terminated v
    )]
]
