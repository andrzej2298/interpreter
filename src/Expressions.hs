module Expressions (eval) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map

import AbsGrammar
import CommonDeclarations


getVariableValue :: Cursor -> String -> EvalMonad
getVariableValue cur x = do
  s <- gets values
  r <- asks variables
  case do
    l <- Map.lookup x r
    Map.lookup l s of
    Just v -> return v
    Nothing -> throwError $ undeclaredVariable x cur

eval :: Expression -> EvalMonad
-- LITERALS
eval (ELitInt _ n) = return $ VInt n
eval (ELitTrue _) = return $ VBool True
eval (ELitFalse _) = return $ VBool False
eval (Neg cur e) = do
  n <- eval e
  case n of
    (VInt v) -> return $ VInt (-v)
    v -> throwError $ TypeError ("can't negate value of type " ++
                                     show v) cur
eval (Not cur e) = do
  b <- eval e
  case b of
    (VBool v) -> return $ VBool (not v)
    v -> throwError $ TypeError ("can't negate value of type " ++
                                     show v) cur
eval (EString _ s) = return $ VString $ filter (/='"') s


eval (EVar cur (Ident x)) = getVariableValue cur x
eval (EApp cur (Ident fn) args) = do
  r <- asks functions
  case Map.lookup fn r of
    Just f -> f args
    Nothing -> throwError $ undeclaredFunction fn cur
-- ARITHMETIC OPERATIONS
eval (EAdd cur e1 op e2) = do
  let opfun = case op of
                (Plus _) -> (+)
                (Minus _) -> (-)
  n1 <- eval e1
  n2 <- eval e2
  case (n1, n2, op) of
    (VInt v1, VInt v2, _) -> return $ VInt $ opfun v1 v2
    (VString v1, VString v2, Plus _) -> return $ VString $ v1 ++ v2
    (f1, f2, _) -> throwError $ TypeError ("can't add or subtract " ++
                                           show f1 ++ " and " ++ show f2) cur
eval (EMul cur e1 op e2) = do
  let opfun = case op of
                (Times _) -> (*)
                (Div _) -> div
                (Mod _) -> mod
  n1 <- eval e1
  n2 <- eval e2
  case (n1, n2, op) of
    (VInt _, VInt 0, Div _) -> throwError $ ArithmeticError "division by zero" cur
    (VInt v1, VInt v2, _) -> return $ VInt $ opfun v1 v2
    (f1, f2, _) -> throwError $ TypeError ("can't multiply or divide " ++
                                           show f1 ++ " and " ++ show f2) cur
eval (ERel cur e1 op e2) = do
  f1 <- eval e1
  f2 <- eval e2
  let opfun = case op of
                (LTH _) -> (<)
                (LE _) -> (<=)
                (GTH _) -> (>)
                (GE _) -> (>=)
                (EQU _) -> (==)
                (NEQ _) -> (/=)
  case (f1, f2, op) of
    (VInt v1, VInt v2, _) -> return $ VBool $ opfun v1 v2
    (VBool v1, VBool v2, EQU _) -> return $ VBool $ v1 == v2
    (VBool v1, VBool v2, NEQ _) -> return $ VBool $ v1 /= v2
    (VString v1, VString v2, EQU _) -> return $ VBool $ v1 == v2
    (VString v1, VString v2, NEQ _) -> return $ VBool $ v1 /= v2
    _ -> throwError $ TypeError ("can't compare " ++ show f1 ++
                                         " and " ++ show f2) cur


eval (EAnd cur e1 e2) = do
  b1 <- eval e1
  b2 <- eval e2
  case (b1, b2) of
    (VBool v1, VBool v2) -> return $ VBool $ v1 && v2
    (f1, f2) -> throwError $ TypeError ("can't perform conjunction of " ++
                                        show f1 ++ " and " ++ show f2) cur
eval (EOr cur e1 e2) = do
  b1 <- eval e1
  b2 <- eval e2
  case (b1, b2) of
    (VBool v1, VBool v2) -> return $ VBool $ v1 || v2
    (f1, f2) -> throwError $ TypeError ("can't perform alterative of " ++
                                        show f1 ++ " and " ++ show f2) cur

eval other = error $ show other
