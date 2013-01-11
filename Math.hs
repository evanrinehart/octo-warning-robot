module Math where

import Data.Maybe
import Data.Either

import Expr
import Value
import Error

calc :: MathOp -> Value -> Value -> Either Error Value
calc Cmp v1 v2 = case comp v1 v2 of
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

comp :: Value -> Value -> Maybe Ordering
comp (Closure _ _) _ = Nothing
comp _ (Closure _ _) = Nothing
comp (Number n1) (Number n2) = Just (compare n1 n2)
comp (Symbol s1) (Symbol s2) = Just (compare s1 s2)
comp (Tuple xs) (Tuple ys) = compareTuple xs ys
comp (Number _) (Symbol _) = Just GT
comp (Symbol _) (Number _) = Just LT
comp (Tuple _) (Symbol _) = Just GT
comp (Symbol _) (Tuple _) = Just LT
comp (Tuple _) (Number _) = Just GT
comp (Number _) (Tuple _) = Just LT

compareTuple :: [Value] -> [Value] -> Maybe Ordering
compareTuple [] [] = Just EQ
compareTuple _ [] = Just GT
compareTuple [] _ = Just LT
compareTuple (x:xs) (y:ys) = case comp x y of
  Nothing -> Nothing
  Just EQ -> compareTuple xs ys
  unequal -> unequal
