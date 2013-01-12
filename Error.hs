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
  FreeVariableError deriving (Show, Typeable)  

instance Exception Error
