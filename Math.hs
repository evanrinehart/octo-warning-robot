module Math where

import Data.Maybe
import Data.Either

import Expr
import Value
import Error

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

math :: MathOp -> (Integer -> Integer -> Integer)
math Add = (+)
math Sub = (-)
math Mul = (*)
math Div = div
math Mod = mod
math Pow = (^)
math Cmp = error "math wrong (should be impossible)"

fromChar :: Char -> MathOp
fromChar c = case c of
  '+' -> Add
  '-' -> Sub
  '*' -> Mul
  '/' -> Div
  '%' -> Mod
  '^' -> Pow
  '#' -> Cmp
  _ -> error "invalid math operator"
