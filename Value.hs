module Value where

import Data.Map
import Expr
import Case

type Env = Map String Value
data Value =
  Symbol String |
  Number Integer |
  Tuple [Value] |
  Closure Env (Case Expr) deriving (Show)

