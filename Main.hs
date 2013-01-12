module Main where

import qualified Data.Map as M

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

  return ()
