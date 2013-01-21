module Message where

import Control.Concurrent
import Data.Maybe

import Value

data React a =
  Normal a |
  SoftCrash a |
  HardCrash a |
  SelfDestruct a |
  Terminated a

data Message a =
  EndOfMessages |
  Message (a, Maybe (MVar (React a)))

data MessagePipe = MessagePipe {
  pipe :: Chan (Message Value),
  callback :: MVar (React Value)
}

send :: MessagePipe -> Value -> IO ()
send mp arg = writeChan (pipe mp) (Message (arg, Nothing))

request :: MessagePipe -> Value -> MessagePipe -> IO (React Value)
request them arg me = do
  writeChan (pipe them) (Message (arg, Just (callback me)))
  takeMVar (callback me)

respond :: Maybe (MVar (React Value)) -> React Value -> IO ()
respond Nothing _ = return ()
respond (Just port) v = putMVar port v

nextMessage :: MessagePipe -> IO (Value, React Value -> IO ())
nextMessage mp = do
  y <- readChan (pipe mp)
  case y of
    EndOfMessages -> nextMessage mp
    Message (arg, mport) -> case mport of
      Nothing -> return (arg, const (return ()))
      Just port -> return (arg, putMVar port)
