module Global where

import Control.Concurrent.MVar
import Data.Map
import qualified Data.Map as M

import Value
import Object

type Global = MVar (Map Value Object)

emptyGlobal :: IO Global
emptyGlobal = newMVar M.empty

