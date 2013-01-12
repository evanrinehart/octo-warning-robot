module Apply where

import Data.Map
import qualified Data.Map as M
import Data.Maybe
import Control.Monad

import Expr
import Value
import Case

apply :: (Case Expr) -> Value -> Maybe (Env, Expr)
apply (Case []) _ = Nothing
apply (Case ((p,expr):cases)) v = case match p v of
  Nothing -> apply (Case cases) v
  Just env -> Just (env, expr)

match :: Pattern -> Value -> Maybe Env
match p v = case p of
  PatVar s -> Just (M.singleton s (Right v))
  PatNum n -> case v of
    Number n' -> if n' == n then Just M.empty else Nothing
    _ -> Nothing
  PatSym y -> case v of
    Symbol y' -> if y' == y then Just M.empty else Nothing
    _ -> Nothing
  PatDontCare -> Just M.empty
  PatTuple pats -> case v of
    Tuple vs -> if length vs /= length pats
      then Nothing
      else do
        envs <- zipWithM match pats vs
        Just (unions envs)
    _ -> Nothing
