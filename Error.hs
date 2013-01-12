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
  SendTargetError |
  ObjectNotFoundError |
  ClosureFieldError |
  ClosureErrorError |
  ErrorError |
  AsyncError |
  FreeVariableError deriving (Show, Typeable)  

instance Exception Error

errorSymbol :: Error -> String
errorSymbol e = case e of
  ArithMismatchError -> "arithmetic-mismatch"
  DivZeroError -> "divide-by-zero"
  PatternMatchError -> "pattern-match-failed"
  ApplyNonClosureError -> "apply-non-closure"
  ClosureMessageError -> "closure-message"
  SendTargetError -> "send-to-closure"
  ObjectNotFoundError -> "object-not-found"
  FreeVariableError -> "undefined-variable"
  ClosureFieldError -> "closure-field-name"
  ClosureErrorError -> "closure-as-error"
  ErrorError -> "error"
  AsyncError -> "asynchronous-error"
