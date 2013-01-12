module Main where

import qualified Data.Map as M
import Control.Concurrent

import Value
import Global
import Object

main :: IO ()
main = do
  g <- newEmptyGlobal
  let env = M.empty

  installGlobalObject g (Symbol "stdout") $ \obj arg -> do
    putStrLn ("STDOUT: " ++ (show arg))
    return (ObjectNormal, Symbol "null")

  port <- newEmptyMVar

  ans <- systemRequest g (Symbol "stdout") (Number 9999) port
  putStrLn ("sent 9999 to object, it responded with "++show ans)

  return ()
