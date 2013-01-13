module Expr where

import Data.Map

import Case
import MathOp

data Expr =
  SymExpr String |
  NumExpr Integer |
  Variable String |
  Cons [Expr] |
  CaseExpr (Case Expr) |
  Apply Expr Expr |
  LetRec Expr (Map String Expr) |
  Maths MathOp Expr Expr |
  Send Expr Expr |
  Request Expr Expr Expr |
  Load Expr Expr |
  Store Expr Expr |
  Error Expr |
  Throw Expr Expr |
  New Expr Expr |
  Halt Expr
    deriving (Show)
