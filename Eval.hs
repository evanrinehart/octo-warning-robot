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

eval :: Global -> Object -> Env -> Expr -> IO Value
eval g o env expr = case expr of
  (SymExpr s) -> return (Symbol s)
  (NumExpr n) -> return (Number n)
  (Variable s) -> case M.lookup s env of
    Nothing -> throw FreeVariableError
    Just v -> return v
  (Cons exprs) -> do
    vs <- forM exprs (eval g o env)
    return (Tuple vs)
  (Apply e1 e2) -> do
    arg <- eval g o env e2
    arg0 <- eval g e env e1
    case arg0 of
      Closure env' c -> do
        (env'', expr) <- apply c arg
        eval g o (unions [env, env', env'']) expr
      _ -> throw NonClosureError
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
  LetRec Expr [(String, Expr)] |
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
