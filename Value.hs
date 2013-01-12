module Value where

import Data.Map
import Expr
import Case

type Env = Map String (Either Expr Value)
data Value =
  Symbol String |
  Number Integer |
  Tuple [Value] |
  Closure Env (Case Expr) deriving (Show)

instance Eq Value where
  v1 == v2 = compare v1 v2 == EQ

instance Ord Value where
  compare v1 v2 = case safeCompareValues v1 v2 of
    Just ans -> ans
    Nothing -> error "can't compare closures"

safeCompareValues = comp
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
