{-# LANGUAGE DeriveDataTypeable #-}
module Error where

import Data.Typeable
import Control.Exception

data Error =
  PatternMatchError |
  ArithMismatchError |
  DivZeroError |
  PowZeroError |
  ApplyMismatchError |
  NonClosureError |
  NonDataError |
  SendTargetError |
  FreeVariableError deriving (Show, Typeable)  

instance Exception Error
