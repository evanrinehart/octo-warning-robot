module Object where

import Control.Concurrent
import Data.Maybe
import Control.Concurrent.Chan
import Data.Map

import Expr
import Error

data Object = Object {
  name     :: Value,
  response :: MVar (Either Error Value),
  fifo     :: Chan (Value, Maybe (MVar Value)),
  handler  :: (Value, Maybe (MVar Value)) -> IO (),
  storage  :: MVar (Map Value Value),
  tid      :: MVar ThreadId
}

newObject :: Global -> String -> Env -> Case -> IO Object
newObject g str env body = do
  msgQ <- mkChan
  port <- newEmptyMVar
  mem <- newMVar M.empty
  tidmv <- newEmptyMVar
  let obj = Object {
  }
  forkIO . forever $ do
    (arg, out) <- readChan msgQ
    case apply body env arg of
      Just (env', expr) -> do
        resp <- eval g obj env' expr -- catch here
        writeMVar out (Right resp)
      Nothing -> do
        writeMVar out (Left PatternMatchError)
  return $ Object {
    name = Symbol str,
    response = port,
    fifo = msgQ,
    handler = undefined,
    storage = undefined,
    tid = undefined
  }
