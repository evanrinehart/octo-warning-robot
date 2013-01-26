{-# LANGUAGE DeriveDataTypeable #-}
module Trap where

import Data.Typeable
import Control.Exception

data AsyncTrap a = AsyncTrap a deriving (Show, Typeable)
data HaltTrap a = HaltTrap a deriving (Show, Typeable)
data ErrorTrap a = ErrorTrap a deriving (Show, Typeable)

instance (Show a, Typeable a) => Exception (AsyncTrap a)
instance (Show a, Typeable a) => Exception (ErrorTrap a)
instance (Show a, Typeable a) => Exception (HaltTrap a)

