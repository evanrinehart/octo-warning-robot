module Main where

import qualified Data.Map as M
import Control.Concurrent
import qualified Data.ByteString as BS

import Value
import Global
import Object
import Repl

main :: IO ()
main = do
  g <- newEmptyGlobal
  installGlobalObject g (Symbol "stdout") $ \obj arg -> do
    putStrLn ("STDOUT: " ++ (showValue arg))
    return (ObjectNormal, Symbol "null")
  src <- BS.getContents
  rep g src
  threadDelay 1000000
  return ()
