module Global where

import Control.Concurrent
import Control.Exception
import Data.Map
import qualified Data.Map as M

import Value
import Object
import Error
import Message

newEmptyGlobal :: IO Global
newEmptyGlobal = newMVar M.empty

lookupGlobal :: Global -> Value -> IO (Maybe Object)
lookupGlobal g v = withMVar g (return . M.lookup v)

installGlobalObject ::
  Global ->
  Value ->
  (Object -> Value -> IO (React Value)) ->
  IO ()
installGlobalObject g newName react = do
  o <- modifyMVar g $ \m -> case M.lookup newName m of
    Nothing -> do
      o' <- newEmptyObject newName
      return (M.insert newName o' m, o')
    Just _ -> throw ObjectExistsError
  startObject g o (react o)

systemRequest ::
  Global -> Value -> Value ->
  MVar (React Value) ->
  IO (React Value)
systemRequest g name arg port = do
  mo <- lookupGlobal g name
  case mo of
    Just o -> do
      let port = callback (io o)
      writeChan (pipe (io o)) (Message (arg, putMVar port))
      takeMVar port
    Nothing -> return undefined

startObject :: Global -> Object -> (Value -> IO (React Value)) -> IO ()
startObject g o react = do
  threadId <- forkIOWithUnmask (objectLoop g o react)
  putMVar (tid o) threadId

globalWrite :: Global -> Value -> Object -> IO Bool
globalWrite g name obj = modifyMVar g w where
  w m = if member name m
    then return (m, False)
    else return (insert name obj m, True)

globalClear :: Global -> Value -> IO ()
globalClear g name = modifyMVar_ g (return . M.delete name)
