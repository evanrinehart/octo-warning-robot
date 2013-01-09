module Global where

import Control.Concurrent.MVar
import Data.Map
import qualified Data.Map as M

import Expr
import Object

type Global = MVar (Map Value Object)

global :: Global -> Value -> IO (Maybe Object)
global mv name = withMVar mv (return . M.lookup name)

emptyGlobal :: IO Global
emptyGlobal = newMVar M.empty
