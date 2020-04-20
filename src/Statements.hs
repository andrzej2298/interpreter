module Statements (execStmt, run) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import qualified Data.Map as Map

import AbsGrammar
import CommonDeclarations
import Expressions (eval)

run :: CursorProgram -> IO ()
run (Program _ l) = do
  evaluated <- execStmt (Sequence l)
  -- HERE
  case evaluated of
    Right _ -> return ()
    Left e -> liftIO $ printError e

execStmt :: Statement -> ExecValue
execStmt i = evalStateT (runExceptT (runReaderT (exec i) r)) s where
  r = emptyEnvironment
  s = emptyStore

processExpValue :: Expression -> (Value -> ExecMonad) -> ExecMonad
processExpValue e f = do
  v <- eval e
  f v

execBlock :: Block Cursor -> ExecMonad
execBlock (Block _ is) = exec (Sequence is)

emptyBlock :: Block Cursor
emptyBlock = Block (0, 0) []

exec :: Statement -> ExecMonad
exec (Empty _) = return ()
exec (Sequence []) = return ()
exec (Sequence ((VarDecl _ varType ds):rest)) = declareVariablesAndExecuteRest varType ds (Sequence rest)
exec (Sequence ((FnDef _ fnType (Ident fnName) args (Block _ body)):rest)) =
  declareFunctionAndExecuteRest fnType fnName args (Sequence body) (Sequence rest)
exec (Sequence (i:is)) = do
  exec i
  exec (Sequence is)
exec d@VarDecl{} = exec (Sequence [d])  -- no instructions are after this variable declaration
exec d@FnDef{} = exec (Sequence [d])  -- no instructions are after this function declaration
exec (Assign cur (Ident x) e) = do
  s <- get
  r <- asks variables
  v <- eval e
  let optLoc = Map.lookup x r
  case optLoc of
    Just l -> put $ Map.insert l v s
    Nothing -> throwError $ undeclaredVariable x cur
exec (Incr cur ident) = exec (Assign cur ident (EAdd cur (EVar cur ident) (Plus cur) (ELitInt cur 1)))
exec (Decr cur ident) = exec (Assign cur ident (EAdd cur (EVar cur ident) (Minus cur) (ELitInt cur 1)))
exec (Print _ e) = processExpValue e printValue where
  printValue (VInt n) = liftIO $ print n
  printValue (VString s) = liftIO $ putStrLn s
  printValue (VBool b) = liftIO $ putStrLn $ map toLower (show b)
exec (SExp _ e) = processExpValue e (const $ return ())
exec (CondElse cur e caseTrue caseFalse) = do
  b <- eval e
  case b of
    (VBool v) -> execBlock $ if v then caseTrue else caseFalse
    other -> throwError $ TypeError ("incompatible type in condition " ++
                                     show other) cur
exec (Cond cur e bl) = exec (CondElse cur e bl emptyBlock)
exec while@(While cur1 e (Block cur2 is)) = exec (CondElse cur1 e
  (Block cur2 (is ++ [while]))
  emptyBlock)

declareVariablesAndExecuteRest :: Type Cursor -> [Item Cursor] -> Statement -> ExecMonad
declareVariablesAndExecuteRest varType items followingInstructions = go items where
  go [] = exec followingInstructions
  go ((Init _ (Ident x) e):rest) = do
    v <- eval e
    bindOneValue x v rest
  go ((NoInit _ (Ident x)):rest) = do
    let v = defaultValue varType
    bindOneValue x v rest
  bindOneValue :: VariableName -> Value -> [Item Cursor] -> ExecMonad
  bindOneValue x v rest = do
    s <- get
    let
      l = alloc s
      s' = Map.insert l v s
    put s'
    local (declareVariable x l) (go rest)

declareFunctionAndExecuteRest :: Type Cursor -> FunctionName -> [Arg Cursor] -> Statement -> Statement -> ExecMonad
declareFunctionAndExecuteRest fnType fnName args body rest =
  local (declareFunction fnName f) (exec rest) where
    f :: Function
    f _ = return (VInt 1)
