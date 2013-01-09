module Expr where

import Case

data Expr =
  SymExpr String |
  NumExpr Integer |
  Variable String |
  Cons [Expr] |
  CaseExpr (Case Expr) |
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
    deriving (Show)

data MathOp = Add | Sub | Mul | Div | Mod | Pow | Cmp
  deriving (Show)