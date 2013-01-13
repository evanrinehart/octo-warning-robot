module Math where

import Data.Maybe
import Data.Either

import Expr
import Value
import Error
import MathOp

calc :: MathOp -> Value -> Value -> Either Error Value
calc Cmp v1 v2 = case safeCompareValues v1 v2 of
  Nothing -> Left ArithMismatchError
  Just o -> case o of
    GT -> Right (Symbol "GT")
    EQ -> Right (Symbol "EQ")
    LT -> Right (Symbol "LT")
calc Div (Number n1) (Number 0) = Left DivZeroError
calc Mod (Number n1) (Number 0) = Left DivZeroError
calc op (Number n1) (Number n2) = (Right . Number) (math op n1 n2)
calc _ _ _ = Left ArithMismatchError
