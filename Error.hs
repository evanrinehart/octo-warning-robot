{-# LANGUAGE DeriveDataTypeable #-}
module Error where

import Data.Typeable
import Control.Exception

data Error =
  ArithMismatchError |
  DivZeroError |
  PatternMatchError |
  ApplyNonClosureError |
  ClosureMessageError |
  ClosureNameError |
  HandlerNotClosureError |
  ObjectNotFoundError |
  ObjectExistsError |
  FreeVariableError deriving (Show, Typeable)  

instance Exception Error

errorSymbol :: Error -> String
errorSymbol e = case e of
  ArithMismatchError -> "arithmetic-mismatch"
  DivZeroError -> "divide-by-zero"
  PatternMatchError -> "pattern-match-failed"
  ApplyNonClosureError -> "apply-non-closure"
  ClosureMessageError -> "closure-message"
  ObjectNotFoundError -> "object-not-found"
  ObjectExistsError -> "object-exists"
  FreeVariableError -> "undefined-variable"
  ClosureNameError -> "closure-as-name"
  HandlerNotClosureError -> "handler-not-closure"
