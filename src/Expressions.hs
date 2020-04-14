module Expressions (evalExp) where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

import AbsGrammar
import CommonDeclarations

evalExp :: Expression -> Environment -> Store -> EvalValue
evalExp e r s = runIdentity (evalStateT (runExceptT (runReaderT (eval e) r)) s)

eval :: Expression -> EvalMonad
--eval _ = return 0
eval (ELitInt _ n) = return n
eval (EAdd _ e1 op e2) = do
  let opfun = case op of
                (Plus _) -> (+)
                (Minus _) -> (-)
  liftM2 opfun (eval e1) (eval e2)
eval (EMul cur e1 op e2) = do
  let opfun = case op of
                (Times _) -> (*)
                (Div _) -> div
                (Mod _) -> mod
  n1 <- eval e1
  n2 <- eval e2
  case (n2, op) of
    (0, (Div _)) -> throwError $ ArithmeticError "division by zero" cur
    _ -> return $ opfun n1 n2
eval (EApp _ (Ident "print") [e]) = do
  n <- eval e
  return n
