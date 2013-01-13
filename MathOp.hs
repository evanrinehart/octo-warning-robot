module MathOp where

data MathOp = Add | Sub | Mul | Div | Mod | Pow | Cmp
  deriving (Show)

math :: MathOp -> (Integer -> Integer -> Integer)
math Add = (+)
math Sub = (-)
math Mul = (*)
math Div = div
math Mod = mod
math Pow = (^)
math Cmp = error "math wrong (should be impossible)"

fromChar :: Char -> MathOp
fromChar c = case c of
  '+' -> Add
  '-' -> Sub
  '*' -> Mul
  '/' -> Div
  '%' -> Mod
  '^' -> Pow
  '#' -> Cmp
  _ -> error "invalid math operator"
