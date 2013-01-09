module Case where

data Case a = Case [(Pattern, a)] deriving (Show)
data Pattern =
  PatDontCare |
  PatVar String |
  PatSym String |
  PatNum Integer |
  PatTuple [Pattern]
    deriving (Show)

