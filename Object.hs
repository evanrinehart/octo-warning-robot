module Object where

import Control.Concurrent
import Control.Concurrent.Chan
import Data.Maybe
import Data.Map
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

startObject :: Object -> (Value -> IO (Either Value Value)) -> IO ()
startObject o react = do
  thread <- forkIO . forever $ do
    (arg, out) <- readChan (fifo o)
    ans <- react arg
    case out of
      Just mv -> putMVar mv ans
      Nothing -> return ()
  putMVar (tid o) thread
