module Math where

import Expr
import Value

calc :: MathOp -> Value -> Value -> Either Error Value
calc Cmp v1 v2 = case comp v1 v2 of
  Nothing -> Left ArithMismatchError
  Just o -> case o of
    GT -> Right (Symbol "GT")
    EQ -> Right (Symbol "EQ")
    LT -> Right (Symbol "LT")
calc op v1 v2 = doMath (math op) v1 v2

doMath ::
  (Integer -> Integer -> Integer) ->
  Value ->
  Value ->
  Either Error Value
doMath f (Number n1) (Number n2) = Right (f n1 n2)
doMath _ _ _ = Left ArithMismatchError

math :: MathOp -> (Integer -> Integer -> Integer)
math Add = (+)
math Sub = (-)
math Mul = (*)
math Div = div
math Mod = mod
math Pow = (^)
math Cmp = error "math wrong"

comp :: Value -> Value -> Maybe Ordering
comp (Closure _ _) _ = Nothing
comp _ (Closure _ _) = Nothing
comp (Number n1) (Number n2) = Just (compare n1 n2)
comp (Symbol s1) (Symbol s2) = Just (compare s1 s2)
comp (Tuple xs) (Tuple ys) = case compare (length xs) (length ys) of
  EQ -> undefined -- blarg
  LT -> Just LT
  GT -> Just GT
comp (Number _) (Symbol _) = Just GT
comp (Symbol _) (Number _) = Just LT
comp (Tuple _) (Symbol _) = Just GT
comp (Symbol _) (Tuple _) = Just LT
comp (Tuple _) (Number _) = Just GT
comp (Number _) (Tuple _) = Just LT
