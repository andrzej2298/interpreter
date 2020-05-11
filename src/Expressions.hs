module Expressions (eval) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import AbsGrammar
import CommonDeclarations



eval :: Expression -> EvalMonad

{---------------------------------------------------------
                    LITERALS
----------------------------------------------------------}
eval (ELitInt _ n) = return $ VInt n
eval (ELitTrue _) = return $ VBool True
eval (ELitFalse _) = return $ VBool False
eval (ETupLit _ exprs) = do
  vals <- mapM eval exprs
  return $ VTuple $ Vector.fromList vals
eval (EArrDef cur t e) = do
  n <- eval e
  let
    err = throwError $ RuntimeError "array size must be a non-negative integer" cur
    arrayOfDefaults v = VArray (Vector.replicate (fromInteger v) (defaultValue t))
  case n of
    (VInt v) -> if v >= 0 then return $ arrayOfDefaults v else err
    _ -> err
eval (EArrExp _ []) = undefined
eval (EArrExp _ exprs) = do
  vals <- mapM eval exprs
  return $ VArray (Vector.fromList vals)
eval (EString _ s) = return $ VString $ filter (/='"') s

{---------------------------------------------------------
                    VARIABLES AND FUNCTIONS
----------------------------------------------------------}
eval (EVar _ (Ident x)) = getVariableValue x
eval (EApp _ (Ident fn) args) =
  case fn of
    "len" -> getArrayLength args
    userFunction -> applyFunction userFunction args
eval (EItemInd cur (Ident x) e) = do
  arr <- getVariableValue x
  n <- eval e
  let
    indexLookup :: Vector.Vector Value -> Integer -> EvalMonad
    indexLookup vec v = case vec Vector.!? fromInteger v of
      Just res -> return res
      Nothing -> throwError $ RuntimeError "array index out of bounds" cur
  case (n, arr) of
    (VInt v, VArray vec) -> indexLookup vec v
    (VInt v, VTuple vec) -> indexLookup vec v
    _ -> undefined

{---------------------------------------------------------
                    ARITHMETIC OPERATIONS
----------------------------------------------------------}
eval (Neg _ e) = do
  n <- eval e
  case n of
    (VInt v) -> return $ VInt (-v)
    _ -> undefined
eval (EAdd _ e1 op e2) = do
  let opfun = case op of
                (Plus _) -> (+)
                (Minus _) -> (-)
  n1 <- eval e1
  n2 <- eval e2
  case (n1, n2, op) of
    (VInt v1, VInt v2, _) -> return $ VInt $ opfun v1 v2
    (VString v1, VString v2, Plus _) -> return $ VString $ v1 ++ v2
    _ -> undefined
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
    _ -> undefined
eval (ERel _ e1 op e2) = do
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
    (_, _, EQU _) -> return $ VBool $ f1 == f2
    (_, _, NEQ _) -> return $ VBool $ f1 /= f2
    _ -> undefined

{---------------------------------------------------------
                    LOGIC OPERATIONS
----------------------------------------------------------}
eval (Not _ e) = do
  b <- eval e
  case b of
    (VBool v) -> return $ VBool (not v)
    _ -> undefined
eval (EAnd _ e1 e2) = do
  b1 <- eval e1
  case b1 of
    -- lazy evaluation of logic expressions
    VBool False -> return $ VBool False
    VBool True -> evalSecond e2
    _ -> undefined
eval (EOr _ e1 e2) = do
  b1 <- eval e1
  case b1 of
    -- lazy evaluation of logic expressions
    VBool True -> return $ VBool True
    VBool False -> evalSecond e2
    _ -> undefined


{---------------------------------------------------------
                    HELPER FUNCTIONS
----------------------------------------------------------}
evalSecond :: Expression -> EvalMonad
evalSecond e = do
  b <- eval e
  case b of
    v@VBool{} -> return v
    _ -> undefined

applyFunction :: FunctionName -> [Expression] -> EvalMonad
applyFunction fn args = do
  r <- asks functions
  case Map.lookup fn r of
    Just f -> f args
    Nothing -> undefined

getArrayLength :: [Expression] -> EvalMonad
getArrayLength [e] = do
  arr <- eval e
  case arr of
    (VArray vec) -> return (VInt (toInteger (length vec)))
    _ -> undefined
getArrayLength _ = undefined

maybeGetVariableValue :: VariableName -> InterpreterMonad (Maybe Value)
maybeGetVariableValue x = do
  s <- gets values
  r <- asks variables
  return $ do
    l <- Map.lookup x r
    Map.lookup l s

getVariableValue :: VariableName -> EvalMonad
getVariableValue x = do
  maybeValue <- maybeGetVariableValue x
  maybe undefined return maybeValue
