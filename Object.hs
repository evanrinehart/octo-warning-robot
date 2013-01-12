module Object where

import Control.Concurrent
import Control.Concurrent.Chan
import Data.Maybe
import Data.Map
import qualified Data.Map as M

import Value
import Expr
import Error

data Object = Object {
  name     :: Value,
  response :: MVar (Either Error Value),
  fifo     :: Chan (Value, Maybe (MVar (Either Error Value))),
  storage  :: MVar (Map Value Value),
  tid      :: MVar ThreadId
}

