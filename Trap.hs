{-# LANGUAGE DeriveDataTypeable #-}
module Trap where

import Data.Typeable
import Control.Exception

data Trap a =
  HaltTrap a |
  ErrorTrap a |
  AsyncTrap a
    deriving (Show, Typeable)

instance (Show a, Typeable a) => Exception (Trap a)

