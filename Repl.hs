module Repl where

import Data.ByteString
import Control.Concurrent

import Parser
import Global
import Value
import Object
import Eval

-- one shot
rep :: Global -> ByteString -> IO ()
rep g src = case parse src of
  Left err -> do
    Prelude.putStrLn err
  Right expr -> do
    installGlobalObject g (Symbol "main") $ \obj _ -> do
      ans <- eval g obj emptyEnv expr
      return (ObjectHalt, ans)
    port <- newEmptyMVar
    ans <- systemRequest g (Symbol "main") (Symbol "run") port
    Prelude.putStrLn (show ans)
    return ()
