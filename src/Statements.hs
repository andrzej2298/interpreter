module Statements (execStmt, run) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable
import Data.Char
import qualified Data.Map as Map

import AbsGrammar
import CommonDeclarations
import Expressions (eval)

run :: CursorProgram -> IO ()
run (Program _ l) = do
  evaluated <- execStmt (Sequence l)
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
exec (Ret _ e) = do
  s <- gets controlValues
  r <- asks control
  v <- eval e
  let loc = Map.lookup Return r
  case loc of
    Just l -> modify (modifyControlStore $ Map.insert l (ReturnValue (Just v)) s)
exec (VRet _) = liftIO $ putStrLn "void return"
exec d@VarDecl{} = exec (Sequence [d])  -- no instructions are after this variable declaration
exec d@FnDef{} = exec (Sequence [d])  -- no instructions are after this function declaration
exec (Assign cur (Ident x) e) = do
  s <- gets values
  r <- asks variables
  v <- eval e
  let optLoc = Map.lookup x r
  case optLoc of
    Just l -> modify (modifyVariableStore $ Map.insert l v s)
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

{---------------------------------------------------------
                    VARIABLES
----------------------------------------------------------}
declareVariablesAndExecuteRest :: Type Cursor -> [Item Cursor] -> Statement -> ExecMonad
declareVariablesAndExecuteRest varType items followingInstructions = do
  s <- gets values
  let
    names = map getName items
    getName (Init _ (Ident x) _) = x
    getName (NoInit _ (Ident x)) = x
    evalValue (Init _ _ e) = eval e
    evalValue (NoInit _ _) = return $ defaultValue varType
  vals <- mapM evalValue items
  let
    generateSingleLocation val (locs, s') = let l = alloc s' in (l:locs, Map.insert l val s')
    (locations, s') = foldr generateSingleLocation ([], s) vals
  modify (modifyVariableStore s')
  local (declareVariables names locations) (exec followingInstructions)


{---------------------------------------------------------
                    FUNCTIONS
----------------------------------------------------------}
declareFunctionAndExecuteRest :: Type Cursor -> FunctionName -> [Arg Cursor] -> Statement -> Statement -> ExecMonad
declareFunctionAndExecuteRest fnType fnName formalArgs body rest = do
  rOfDeclaration <- ask
  let
    f :: Function
    f actualArgs = do
      variableEnvOfApplication <- asks variables
      (env, retLocation) <- getFunctionEnvironmentAndReturnLocation fnName f variableEnvOfApplication rOfDeclaration formalArgs actualArgs
      -- liftIO $ print env
      local (const env) (exec body)
      s <- gets controlValues
      case (Map.lookup retLocation s, fnType) of
        (Just (ReturnValue (Just v)), _) -> return v
        (_, Void _) -> return VoidValue
        _ -> undefined
  local (declareFunction fnName f) (exec rest)

getFunctionEnvironmentAndReturnLocation :: FunctionName -> Function -> VariableEnvironment -> Environment ->
  [Arg Cursor] -> [Expression] -> InterpreterMonad (Environment, Location)
getFunctionEnvironmentAndReturnLocation fnName f variableEnvOfApplication rOfDeclaration formalArgs actualArgs = do
  s <- gets controlValues
  argumentsDeclaredEnv <- getEnvironmentWithActualArgs formalArgs actualArgs variableEnvOfApplication rOfDeclaration
  let
    resultEnvironment = declareReturnValue retLocation functionAndArgumentsDeclaredEnv
    functionAndArgumentsDeclaredEnv = declareFunction fnName f argumentsDeclaredEnv
    retLocation = alloc s
  return (resultEnvironment, retLocation)

getEnvironmentWithActualArgs :: [Arg Cursor] -> [Expression] -> VariableEnvironment -> Environment -> InterpreterMonad Environment
getEnvironmentWithActualArgs formalArgs actualArgs variableEnvOfApplication rOfDeclaration = do
  s <- gets values
  let
    getName (ArgVal _ _ (Ident fn)) = fn
    getName (ArgRef _ _ (Ident fn)) = fn
    argNames = map getName formalArgs
    generateSingleLocation (ArgRef{}, (EVar cur (Ident xActual))) (locs, s') =
      case Map.lookup xActual variableEnvOfApplication of
        Just l -> return (l:locs, s')
        Nothing -> throwError $ undeclaredVariable xActual cur
    generateSingleLocation (ArgVal{}, e) (locs, s') = do
      let l = alloc s'
      val <- eval e
      return (l:locs, Map.insert l val s')
    generateSingleLocation (ArgRef{}, e) _ = throwError $ TypeError "" (getCursor e)
  (argLocations, s') <- foldrM generateSingleLocation ([], s) (zip formalArgs actualArgs)
  modify (modifyVariableStore s')
  return $ declareVariables argNames argLocations rOfDeclaration
