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
import Message

data Object = Object {
  name     :: Value,
  io       :: MessagePipe,
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
  mp  <- newMessagePipe
  chan <- newChan
  return $ Object {
    name = newName,
    io = mp,
    storage = mv2,
    tid = mv3
  }

objectStart :: Global -> Value -> (Value -> IO (React Value)) -> IO ()
objectStart g name react = modifyMVar_ g $ \gmap -> do
  obj <- newEmptyObject name
  gmap' <- if member name gmap
    then throw ObjectExistsError
    else return (insert name obj gmap)
  forkIO $ onException
    (objectLoop g obj react)
    (objectEnd g obj (Symbol "unknown-exception"))
  return gmap'

objectLoop ::
  Global ->
  Object ->
  (Value -> IO (React Value)) ->
  IO ()
objectLoop g obj react = do
  let loop  = objectLoop g obj react
  let die = objectEnd g obj
  y <- bracketOnError
    (nextMessage (io obj))
    (\(_, out) -> out (HardCrash (Symbol "unexplained")))
    (\(arg, out) -> mask $ \restore -> do
      y <- restore (react arg) `catches` objectCatches
      out y
      return y)
  case y of
    Normal _       -> loop
    SoftCrash _    -> loop
    HardCrash v    -> die v
    SelfDestruct v -> die v
    Terminated v   -> die v

-- the above will not work.
-- if an unknown (probably async) exception
-- occurs in react, it will not be caught by objectCatches.
-- the top level handler will run finally, but it will not respond
-- to the waiting object (out)

-- use a bracket to write unknown-exception to the output


{- BLARG

a demon is created and spends a period of time getting to the point where
he is waiting on his chan for messages. before that occurs an asynch exception
can occur, hell get kill, and nothing bad will happen.

when waiting on the chan he can get an async exception, he will die and nothing
bad will happen.

immediately after getting a message and before putting the response we have
danger of race condition.

I. when a demon gets killed for any reason he must be removed from the global
object directory

II. when a demon gets killed for any reason all requests waiting for him need
to be cancelled by responding to them.

III. when a demon is in the process of handle a message and gets killed the
message he is responding to must be cancelled by responding to it.

-}
  
globalWrite g name obj

objectEnd :: Global -> Object -> Value -> IO ()
objectEnd g obj v = modifyMVar_ g $ \gmap -> do
  msgs <- messageDump (io obj)
  forM_ msgs $ \(_, mport) -> respond mport v
  return (M.delete (name obj) gmap)

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

