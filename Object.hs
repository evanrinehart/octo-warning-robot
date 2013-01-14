module Object where

import Control.Concurrent
import Control.Concurrent.Chan
import Data.Maybe
import Data.Map
import Control.Monad.Fix
import Control.Monad
import qualified Data.Map as M

import Value
import Expr
import Error

data Object = Object {
  name     :: Value,
  response :: MVar (Either Value Value),
  fifo     :: Chan (Value, Maybe (MVar (Either Value Value))),
  storage  :: MVar (Map Value Value),
  tid      :: MVar ThreadId
}

data ObjectCondition =
  ObjectNormal |
  ObjectError |
  ObjectHalt
    deriving (Show)

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

startObject :: Object -> (Value -> IO (ObjectCondition, Value)) -> IO ()
startObject o react = do
  thread <- forkIO . fix $ \loop -> do
    (arg, out) <- readChan (fifo o)
    (cond, ans) <- react arg
    case out of
      Just mv -> case cond of
        ObjectNormal -> putMVar mv (Right ans)
        _ -> putMVar mv (Left ans)
      Nothing -> return ()
    case cond of
      ObjectNormal -> loop
      ObjectError -> do
        me <- myThreadId
        killThread me
      ObjectHalt -> do
        thread <- myThreadId
        putStrLn ((showValue (name o))++" ended normally.")
        return ()
  putMVar (tid o) thread


