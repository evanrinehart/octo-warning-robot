module Case where

data Case a = Case [(Pattern, a)] deriving (Show)
data Pattern =
  Anything String |
  DontCare |
  TuplePattern [Pattern] |
  SymPattern String |
  NumPattern Integer deriving (Show)

