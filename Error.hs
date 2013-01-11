{-# LANGUAGE DeriveDataTypeable #-}
module Error where

import Data.Typeable
import Control.Exception

data Error =
  ArithMismatchError |
  DivZeroError |
  PatternMatchError |
  ApplyMismatchError |
  NonClosureError |
  NonDataError |
  SendTargetError |
  FreeVariableError deriving (Show, Typeable)  

instance Exception Error
