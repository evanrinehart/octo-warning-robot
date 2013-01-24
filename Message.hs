module Message where

import Control.Concurrent
import Data.Maybe
import Control.Exception
import qualified Control.Exception as E

import Value
import Trap
import Util

data React a =
  Normal a |
  SoftCrash a |
  HardCrash a |
  SelfDestruct a |
  Terminated a

data Message a =
  EndOfMessages |
  Message (a, React a -> IO ())

data MessagePipe = MessagePipe {
  pipe :: Chan (Message Value),
  callback :: MVar (React Value)
}

valueOf :: React Value -> Value
valueOf r = case r of
  Normal v -> v
  SoftCrash v -> v
  HardCrash v -> v
  SelfDestruct v -> v
  Terminated v -> v

newMessagePipe :: IO MessagePipe
newMessagePipe = do
  mv <- newEmptyMVar
  ch <- newChan
  return (MessagePipe ch mv)

send :: MessagePipe -> Value -> IO ()
send mp arg = writeChan (pipe mp) (Message (arg, doNothingWith))

request :: MessagePipe -> Value -> MessagePipe -> IO (React Value)
request them arg me = do
  let port = callback me
  writeChan (pipe them) (Message (arg, putMVar port))
  takeMVar port

respond :: Maybe (MVar (React Value)) -> React Value -> IO ()
respond Nothing _ = return ()
respond (Just port) v = putMVar port v

nextMessage ::
  MessagePipe ->
  (Value -> IO ()) ->
  IO (Value, React Value -> IO ())
nextMessage mp die = do
  y <- E.catch
    (readChan (pipe mp))
    (\ex@(AsyncTrap v) -> die v >> throw ex)
  case y of
    EndOfMessages -> nextMessage mp die
    Message (arg, out) -> return (arg, out)

messageDump ch = do
  writeChan ch EndOfMessages
  let isEndOfMessages x = case x of
        EndOfMessages -> True
        _ -> False
  msgs <- readUntil ch isEndOfMessages []
  return msgs

readUntil :: Chan a -> (a -> Bool) -> [a] -> IO [a]
readUntil ch done accum = do
  y <- readChan ch
  if done y
    then return accum
    else do
      rest <- readUntil ch done accum
      return (accum ++ rest)
