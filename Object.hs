{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
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

type Global = MVar (Map Value Object)

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


objectLoop ::
  Global ->
  Object ->
  (Value -> IO (React Value)) ->
  (forall a. IO a -> IO a) ->
  IO ()
objectLoop g obj react unmask = do
  let loop = objectLoop g obj react unmask
  let die = objectEnd g obj
  (arg, out) <- nextMessage (io obj) die
  y <- unmask (react arg) `catches` objectCatches
  out y
  case y of
    Normal _       -> loop
    SoftCrash _    -> loop
    HardCrash v    -> die v
    SelfDestruct v -> die v
    Terminated v   -> die v

objectEnd :: Global -> Object -> Value -> IO ()
objectEnd g obj v = return () --modifyMVar_ g $ \gmap -> do
  --msgs <- messageDump (io obj)
  --forM_ msgs $ \(_, mport) -> respond mport v
  --return (M.delete (name obj) gmap)

objectCatches :: [Handler (React Value)]
objectCatches =
  [Handler (\(ex :: Error) -> (return . SoftCrash . fromError) ex),
   Handler (\(HaltTrap v) -> return (SelfDestruct v)),
   Handler (\(ErrorTrap v)-> return (SoftCrash v)),
   Handler (\(AsyncTrap v) -> return (Terminated v))]

