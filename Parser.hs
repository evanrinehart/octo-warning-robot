{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Data.Attoparsec
import qualified Data.Attoparsec as A
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8 as C8
import Data.ByteString hiding (elem)
import Data.ByteString.UTF8
import Data.Char
import Data.Char as C
import Control.Monad
import qualified Data.Map as M

import Expr
import Case
import MathOp

parse :: ByteString -> Either String Expr
parse input = parseOnly parser input

parser :: Parser Expr
parser = do
  skipSpace
  e <- parseExpr
  skipSpace
  endOfInput
  return e

parseExpr :: Parser Expr
parseExpr = choice
  [fmap NumExpr numberExpr
  ,consExpr
  ,caseExpr
  ,fmap Variable variable
  ,apply
  ,mathExpr
  ,letrec
  ,send
  ,request
  ,load
  ,store
  ,halt
  ,errorExpr
  ,throwExpr
  ,new
  ,fmap SymExpr symbol]

send :: Parser Expr
send = do
  string "send{"
  skipSpace
  arg0 <- parseExpr
  skipSpace
  char ','
  skipSpace
  arg1 <- parseExpr
  skipSpace
  char '}'
  return (Send arg0 arg1)

request :: Parser Expr
request = do
  string "request{"
  skipSpace
  arg0 <- parseExpr
  skipSpace
  char ','
  skipSpace
  arg1 <- parseExpr
  skipSpace
  handler <- option
    defaultHandler
    (do
      char ','
      skipSpace
      parseExpr)
  skipSpace
  char '}'
  return (Request arg0 arg1 handler)

load = commandPair "load" Load
store = commandPair "store" Store
halt = command "halt" Halt
errorExpr = command "error" Error
throwExpr = commandPair "throw" Throw
new = commandPair "new" New

command name constructor = do
  string name
  char '{'
  skipSpace
  arg <- parseExpr
  skipSpace
  char '}'
  return (constructor arg)

commandPair name constructor = do
  string name
  char '{'
  skipSpace
  arg1 <- parseExpr
  skipSpace
  char ','
  skipSpace
  arg2 <- parseExpr
  skipSpace
  char '}'
  return (constructor arg1 arg2)

defaultHandler :: Expr
defaultHandler = (CaseExpr . Case)
  [(PatVar "x", Error (Variable "x"))]

consExpr :: Parser Expr
consExpr = fmap Cons $ enclosedInSepBy
  (char '(')
  parseExpr
  skipSpace
  (char ')')

caseExpr :: Parser Expr
caseExpr = fmap (CaseExpr . Case) $ enclosedInSepBy
  (string "case{")
  (caseElement)
  (skipSpace >> char ';' >> skipSpace)
  (char '}')

caseElement :: Parser (Pattern, Expr)
caseElement = separatedPair
  pattern
  (skipSpace >> string "->" >> skipSpace)
  parseExpr

pattern :: Parser Pattern
pattern = choice
  [fmap PatSym symbol
  ,fmap PatNum numberExpr
  ,fmap PatVar variable
  ,fmap PatTuple
    (enclosedInSepBy (char '(') pattern skipSpace (char ')'))]

variable :: Parser String
variable = do
  char '$'
  symbol

apply :: Parser Expr
apply = do
  char '['
  arg0 <- parseExpr
  skipSpace
  arg1 <- parseExpr
  char ']'
  return (Apply arg0 arg1)

mathExpr :: Parser Expr
mathExpr = do
  char '['
  skipSpace
  op <- (fmap MathOp.fromChar . C8.satisfy . C8.inClass) "-+*/%^#"
  skipSpace
  arg1 <- parseExpr
  skipSpace
  arg2 <- parseExpr
  skipSpace
  char ']'
  return (Maths op arg1 arg2)

letrec :: Parser Expr
letrec = do
  string "letrec("
  skipSpace
  e <- parseExpr
  skipSpace
  char ')'
  defs <- enclosedInSepBy
    (char '{')
    (separatedPair
      variable
      (skipSpace >> char '=' >> skipSpace)
      parseExpr
    )
    (skipSpace >> char ';' >> skipSpace)
    (char '}')
  return (LetRec e (M.fromList defs))

symbol :: Parser String
symbol = do
  first <- anyChar
  when
    (not (validSymbolChar0 first))
    (fail $ "symbol can't start with "++[first]++" character")
  rest <- fmap toString (C8.takeWhile validSymbolChar)
  let whole = first:rest
  when
    (whole `elem` ["+","-","*","/","%","^","#"])
    (fail $ whole++" can't be a symbol")
  return (first:rest)

validSymbolChar :: Char -> Bool
validSymbolChar c = validSymbolChar0 c || C.isDigit c || c `elem` "+-"

validSymbolChar0 :: Char -> Bool
validSymbolChar0 c = isAlpha c || c `elem` "!\"#%&'*./:<>?\\^_|~@"

numberExpr :: Parser Integer
numberExpr = signed decimal

separatedPair :: Parser l -> Parser s -> Parser r -> Parser (l,r)
separatedPair l s r = do
  x <- l
  s
  y <- r
  return (x,y)

enclosedInSepBy ::
  Parser l -> Parser a -> Parser s -> Parser r -> Parser [a]
enclosedInSepBy l p s r = do
  l
  xs <- sepBy p s
  r
  return xs

