{-# LANGUAGE ScopedTypeVariables #-}
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
import Trap
import Message

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
    clo <- eval g o env e1
    applyClosure g o env clo arg
  LetRec expr' env' -> eval g o (env `union` fmap Left env') expr'
  Maths op e1 e2 -> do
    arg1 <- eval g o env e1
    arg2 <- eval g o env e2
    case calc op arg1 arg2 of
      Left err -> throw err
      Right v -> return v
  Send e1 e2 -> withObject g o env e1 e2
    (\o' v -> do
      send (io o') v
      return (Symbol "delivered"))
    (return (Symbol "nobody-there"))
  Request e1 e2 e3 -> do
    v  <- eval g o env e2
    name  <- eval g o env e1
    mo' <- lookupGlobal g name
    case mo' of
      Nothing -> throw ObjectNotFoundError
      Just o' -> do
        resp <- request (io o') v (io o)
        case resp of
          Normal v' -> return v'
          problem -> applyValue g o env e3 (valueOf problem)
  Load e1 e2 -> do
    field <- eval g o env e1
    when (isClosure field) (throw ClosureNameError)
    mo' <- objectLookup o field
    case mo' of
      Nothing -> eval g o env e2
      Just v -> return v
  Store e1 e2 -> do
    field <- eval g o env e1
    when (isClosure field) (throw ClosureNameError)
    arg <- eval g o env e2
    objectStore o field arg
    return arg
  Error e1 -> do
    err <- eval g o env e1
    if isClosure err
      then throw ClosureNameError
      else throw (ErrorTrap err)
  Throw e1 e2 -> withObject g o env e1 e2
    (\o' err -> do
      thread <- readMVar (tid o')
      when (isClosure err) (throw ClosureNameError)
      throwTo thread (AsyncTrap err)
      return (Tuple []))
    (throw ObjectNotFoundError)
  New e1 e2 -> do
    clo <- eval g o env e2
    newName <- eval g o env e1
    when (notClosure clo) (throw HandlerNotClosureError)
    when (isClosure newName) (throw ClosureNameError)
    installGlobalObject g newName (userObject g clo)
    return (Symbol "object-created")
  Halt e1 -> do
    v <- eval g o env e1
    when (isClosure v) (throw ClosureNameError)
    throw (HaltTrap v)

applyValue :: Global -> Object -> Env -> Expr -> Value -> IO Value
applyValue g o env expr arg = do
  clo <- eval g o env expr
  applyClosure g o env clo arg

applyClosure :: Global -> Object -> Env -> Value -> Value -> IO Value
applyClosure g o env clo arg = case clo of
  Closure env' c -> case apply c arg of
    Just (env'', expr') -> eval g o (unions [env'', env', env]) expr'
    Nothing -> throw PatternMatchError
  _ -> throw ApplyNonClosureError

userObject ::
  Global -> Value -> Object -> Value -> IO (React Value)
userObject g clo o arg = fmap Normal $ applyClosure g o M.empty clo arg

withObject g o env e1 e2 success failure = do
  arg0 <- eval g o env e1
  case arg0 of
    Closure _ _ -> throw ClosureNameError
    target -> do
      mo' <- lookupGlobal g target
      case mo' of
        Just o' -> do
          arg <- eval g o env e2
          case arg of
            Closure _ _ -> throw ClosureMessageError
            value -> success o' value
        Nothing -> failure
