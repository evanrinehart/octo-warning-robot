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

  let bootstrap = Closure M.empty (Case [(PatSym "run", prog)])

  installGlobalObject g (Symbol "main") (userObject g bootstrap)

  port <- newEmptyMVar
  ans <- systemRequest g (Symbol "main") (Symbol "run") port
  case ans of
    Left err -> putStrLn ("ERROR: "++err)
    Right v -> putStrLn (showValue v)
