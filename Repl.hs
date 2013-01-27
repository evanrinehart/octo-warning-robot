{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Repl where

import Data.ByteString hiding (putStrLn, putStr)
import Data.ByteString.UTF8 (fromString)
import Control.Concurrent
import System.Console.Readline
import Data.Attoparsec
import System.Exit
import Control.Exception
import Data.Monoid

import Parser
import Global
import Value
import Object
import Eval
import Expr
import Error
import Trap
import Message


repl :: (Expr -> IO ()) -> IO ()
repl exec = do
  l <- fmap (fmap fromString) $ readline "?? "
  l <- case l of
    Nothing -> putStrLn "\nexit" >> exitSuccess
    Just l -> return (l <> " ")
  let
    r = case parse parser l of
      f@(Fail _ _ _) -> f
      p@(Partial _) -> case feed p "" of
        f@(Fail _ _ _) -> p
        d@(Done _ _) -> d
  x <- readExpr r
  case x of
    Left (cs, m) -> do
      putStrLn ("SYNTAX ERROR: "++m)
      putStrLn cs
    Right expr -> exec expr
  repl exec

readExpr :: Result Expr -> IO (Either (String,String) Expr)
readExpr r = case r of
  Fail _ cs m -> return (Left (unlines cs, m))
  Partial cont -> do
    ml <- fmap (fmap fromString) $ readline ".. "
    l <- case ml of
      Nothing -> putStrLn "\nexit" >> exitSuccess
      Just l -> return (l <> " ")
    let
      r' = case feed r l of
        f@(Fail _ _ _) -> f
        p@(Partial _) -> case feed p "" of
          f@(Fail _ _ _) -> p
          d@(Done _ _) -> d
    readExpr r'
  Done _ expr -> return (Right expr)


replObject ::
  Global -> MVar Value -> Object -> Value -> IO (React Value)
replObject g loader o arg = do
    clo <- takeMVar loader
    x <- applyClosure g o emptyEnv clo arg
    return (Normal x)
