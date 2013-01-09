module Eval where

import Data.Map
import qualified Data.Map as M
import Data.Either
import Data.Maybe
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad

import Expr
import Object
import Global
import Error

eval :: Global -> Object -> Env -> Expr -> IO Value
eval _ _ _ (SymExpr s) = return (Symbol s)
eval _ _ _ (NumExpr n) = return (Number n)
eval _ _ e (Variable s) = case M.lookup s e of
  Nothing -> throw FreeVariableError
  Just v -> return v
eval g o e (Cons exprs) = do
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
