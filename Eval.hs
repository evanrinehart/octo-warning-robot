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
eval g o e expr = case expr of
  (SymExpr s) -> return (Symbol s)
  (NumExpr n) -> return (Number n)
  (Variable s) -> case M.lookup s e of
    Nothing -> throw FreeVariableError
    Just v -> return v
  (Cons exprs) -> do
    vs <- forM exprs (eval g o e)
    return (Tuple vs)

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
