module Eval where

import Data.Map
import qualified Data.Map as M
import Data.Either
import Data.Maybe
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad

import Value
import Expr
import Object
import Global
import Error
import Apply
import Math

eval :: Global -> Object -> Env -> Expr -> IO Value
eval g o env expr = case expr of
  SymExpr s -> return (Symbol s)
  NumExpr n -> return (Number n)
  Variable s -> case M.lookup s env of
    Just (Right val) -> return val
    Just (Left expr') -> eval g o env expr'
    Nothing -> throw FreeVariableError
  Cons exprs -> do
    vs <- forM exprs (eval g o env)
    return (Tuple vs)
  CaseExpr c -> return (Closure env c)
  Apply e1 e2 -> do
    arg <- eval g o env e2
    arg0 <- eval g o env e1
    case arg0 of
      Closure env' c -> case apply c arg of
        Just (env'', expr') -> eval g o (unions [env, env', env'']) expr'
        Nothing -> throw PatternMatchError
      _ -> throw ApplyNonClosureError
  LetRec expr' env' -> eval g o (env `union` fmap Left env') expr'
  Maths op e1 e2 -> do
    arg1 <- eval g o env e1
    arg2 <- eval g o env e2
    case calc op arg1 arg2 of
      Left err -> throw err
      Right v -> return v
  Send e1 e2 -> withObject g o env e1 e2
    (\o' v -> do
      writeChan (fifo o') (v, Nothing)
      return (Symbol "delivered"))
    (return (Symbol "nobody-there"))
  Request e1 e2 e3 -> withObject g o env e1 e2
    (\o' v -> do
      writeChan (fifo o') (v, Just (response o))
      resp <- takeMVar (response o)
      case resp of
        Right v' -> return v'
        Left err -> eval g o env (Apply e3 (SymExpr (errorSymbol err))))
    (throw ObjectNotFoundError)
  Load e1 -> undefined
  Store e1 e2 -> undefined
  Error e1 -> undefined
  Throw e1 e2 -> undefined
  New e1 e2 -> undefined
  Halt e1 -> undefined

withObject g o env e1 e2 success failure = do
  arg0 <- eval g o env e1
  case arg0 of
    Closure _ _ -> throw SendTargetError
    target -> do
      mo' <- lookupGlobal g target
      case mo' of
        Just o' -> do
          arg <- eval g o env e2
          case arg of
            Closure _ _ -> throw ClosureMessageError
            value -> success o' value
        Nothing -> failure
