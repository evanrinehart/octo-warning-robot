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
import Control.Applicative

import Expr
import Case
import MathOp

parse :: ByteString -> Either String Expr
parse input = parseOnly parser input

parser :: Parser Expr
parser = do
  skipSpaceNL
  e <- parseExpr
  skipSpaceNL
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
  skipSpaceNL
  arg0 <- parseExpr
  skipSpaceNL
  char ','
  skipSpaceNL
  arg1 <- parseExpr
  skipSpaceNL
  char '}'
  return (Send arg0 arg1)

request :: Parser Expr
request = do
  string "request{"
  skipSpaceNL
  arg0 <- parseExpr
  skipSpaceNL
  char ','
  skipSpaceNL
  arg1 <- parseExpr
  skipSpaceNL
  handler <- option
    defaultHandler
    (do
      char ','
      skipSpaceNL
      parseExpr)
  skipSpaceNL
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
  skipSpaceNL
  arg <- parseExpr
  skipSpaceNL
  char '}'
  return (constructor arg)

commandPair name constructor = do
  string name
  char '{'
  skipSpaceNL
  arg1 <- parseExpr
  skipSpaceNL
  char ','
  skipSpaceNL
  arg2 <- parseExpr
  skipSpaceNL
  char '}'
  return (constructor arg1 arg2)

defaultHandler :: Expr
defaultHandler = (CaseExpr . Case)
  [(PatVar "x", Error (Variable "x"))]

consExpr :: Parser Expr
consExpr = fmap Cons $ enclosedInSepBy
  (char '(')
  parseExpr
  skipSpaceNL
  (char ')')

caseExpr :: Parser Expr
caseExpr = fmap (CaseExpr . Case) $ enclosedInSepBy
  (string "case{")
  (caseElement)
  (skipSpaceNL >> char ';' >> skipSpaceNL)
  (char '}')

caseElement :: Parser (Pattern, Expr)
caseElement = separatedPair
  pattern
  (skipSpaceNL >> string "->" >> skipSpaceNL)
  parseExpr

pattern :: Parser Pattern
pattern = choice
  [fmap PatSym symbol
  ,fmap PatNum numberExpr
  ,fmap PatVar variable
  ,fmap PatTuple
    (enclosedInSepBy (char '(') pattern skipSpaceNL (char ')'))]

variable :: Parser String
variable = do
  char '$'
  symbol

apply :: Parser Expr
apply = do
  char '['
  arg0 <- parseExpr
  skipSpaceNL
  arg1 <- parseExpr
  char ']'
  return (Apply arg0 arg1)

mathExpr :: Parser Expr
mathExpr = do
  char '['
  skipSpaceNL
  op <- (fmap MathOp.fromChar . C8.satisfy . C8.inClass) "-+*/%^#"
  skipSpaceNL
  arg1 <- parseExpr
  skipSpaceNL
  arg2 <- parseExpr
  skipSpaceNL
  char ']'
  return (Maths op arg1 arg2)

letrec :: Parser Expr
letrec = do
  string "letrec("
  skipSpaceNL
  mainExpr <- parseExpr
  skipSpaceNL
  string "){"
  skipSpaceNL
  defs <- flip sepBy (skipSpaceNL >> char ';' >> skipSpaceNL) $ do
    v <- variable
    skipSpaceNL
    char '='
    skipSpaceNL
    e <- parseExpr
    return (v, e)
  skipSpaceNL
  char '}'
  return (LetRec mainExpr (M.fromList defs))

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

skipSpaceNL :: Parser ()
skipSpaceNL = do
  C8.takeWhile C.isSpace
  return ()
