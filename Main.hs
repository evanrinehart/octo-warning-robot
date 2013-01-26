module Main where

import qualified Data.Map as M
import Control.Concurrent
import qualified Data.ByteString as BS
import System.Exit

import Value
import Global
import Object
import Parser
import Eval
import Case
import Repl
import Message

main :: IO ()
main = do
  g <- newEmptyGlobal

  installGlobalObject g (Symbol "stdout") $ \obj arg -> do
    putStrLn ("STDOUT: " ++ (showValue arg))
    (return . Normal . Symbol) "null"

  loader <- newEmptyMVar
  installGlobalObject g (Symbol "main") (replObject g loader)

  port <- newEmptyMVar
  repl $ \expr -> do
    putMVar loader (Closure emptyEnv (Case [(PatSym "run", expr)]))
    ans <- systemRequest g (Symbol "main") (Symbol "run") port
    case ans of
      Normal v -> do
        putStrLn ("==> " ++ showValue v)
        putStrLn ""
      SelfDestruct v -> do
        putStrLn (promptOf (SelfDestruct v) ++ showValue v)
        exitSuccess
      problem -> do
        putStrLn (promptOf problem ++ (showValue . valueOf) problem)
        putStrLn ""
