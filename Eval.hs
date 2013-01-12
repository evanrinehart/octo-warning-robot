module Eval where

import Data.Map
import qualified Data.Map as M
import Data.Either
import Data.Maybe
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad

import Value
import Expr
import Object
import Global
import Error
import Apply
import Math

eval :: Global -> Object -> Env -> Expr -> IO Value
eval g o env expr = case expr of
  (SymExpr s) -> return (Symbol s)
  (NumExpr n) -> return (Number n)
  (Variable s) -> case M.lookup s env of
    Just (Right val) -> return val
    Just (Left expr') -> eval g o env expr'
    Nothing -> throw FreeVariableError
  (Cons exprs) -> do
    vs <- forM exprs (eval g o env)
    return (Tuple vs)
  (CaseExpr c) -> return (Closure env c)
  (Apply e1 e2) -> do
    arg <- eval g o env e2
    arg0 <- eval g o env e1
    case arg0 of
      Closure env' c -> case apply c arg of
        Just (env'', expr') -> eval g o (unions [env, env', env'']) expr'
        Nothing -> throw PatternMatchError
      _ -> throw ApplyNonClosureError
  (LetRec expr' env') -> eval g o (env `union` fmap Left env') expr'
  (Maths op e1 e2) -> do
    arg1 <- eval g o env e1
    arg2 <- eval g o env e2
    case calc op arg1 arg2 of
      Left err -> throw err
      Right v -> return v
  


{-
  SymExpr String |
  NumExpr Integer |
  Variable String |
  Cons [Expr] |
  CaseExpr Case |
  Apply Expr Expr |
  LetRec Expr (Map String Expr) |
  Maths MathOp Expr Expr |
  Send Expr Expr |
  Request Expr Expr Expr |
  Load Expr |
  Store Expr Expr |
  Error Expr |
  Throw Expr Expr |
  New Expr Expr |
  Halt Expr
-}
