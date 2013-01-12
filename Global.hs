module Global where

import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Exception
import Data.Map
import qualified Data.Map as M

import Value
import Object
import Error

type Global = MVar (Map Value Object)

newEmptyGlobal :: IO Global
newEmptyGlobal = newMVar M.empty

lookupGlobal :: Global -> Value -> IO (Maybe Object)
lookupGlobal g v = withMVar g (return . M.lookup v)

installGlobalObject ::
  Global ->
  Value ->
  (Object -> Value -> IO (ObjectCondition, Value)) ->
  IO ()
installGlobalObject g newName react = do
  o <- modifyMVar g $ \m -> case M.lookup newName m of
    Nothing -> do
      o' <- newEmptyObject newName
      return (M.insert newName o' m, o')
    Just _ -> throw ObjectExistsError
  startObject o (react o)

systemRequest ::
  Global -> Value -> Value ->
  MVar (Either Value Value) ->
  IO (Either String Value)
systemRequest g name arg port = do
  mo <- lookupGlobal g name
  case mo of
    Just o -> do
      writeChan (fifo o) (arg, Just port)
      response <- takeMVar port
      case response of
        Left err -> (return . Left . show) err
        Right val -> return (Right val)
    Nothing -> return (Left "Object not found")
