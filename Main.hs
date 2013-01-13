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

main :: IO ()
main = do
  g <- newEmptyGlobal
  src <- BS.getContents
  prog <- case parse src of
    Left err -> do
      putStrLn err
      exitFailure
    Right prog -> return prog

  installGlobalObject g (Symbol "stdout") $ \obj arg -> do
    putStrLn ("STDOUT: " ++ (showValue arg))
    return (ObjectNormal, Symbol "null")

  installGlobalObject g (Symbol "main") $ \obj _ -> do
    ans <- eval g obj emptyEnv prog
    return (ObjectNormal, ans)

  port <- newEmptyMVar
  ans <- systemRequest g (Symbol "main") (Symbol "run") port
  case ans of
    Left err -> putStrLn ("MainMain: ERROR "++err)
    Right v -> putStrLn ("MainMain: exit value "++showValue v)
  threadDelay 1000000
