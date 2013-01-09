module Expr where

import Data.Either
import Data.Map

import Math

type Env = Map String Value

data Value =
  Symbol String |
  Number Integer |
  Tuple [Value] |
  Closure Env Case deriving (Eq, Ord, Show)

data Case = Case [(Pattern, Expr)]
  deriving (Eq, Ord, Show)

data Pattern =
  Anything String |
  DontCare |
  TuplePattern [Pattern] |
  SymPattern String |
  NumPattern Integer deriving (Eq, Ord, Show)

data Expr =
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
    deriving (Eq, Ord, Show)

data MathOp = Add | Sub | Mul | Div | Mod | Pow | Cmp
  deriving (Eq, Ord, Show)
