module Statements (execStmt, run) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable
import qualified Data.Map as Map
import qualified Data.Vector as Vector

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


-- check if a jump statement has been issued (return / break / continue)
-- if so, ignores the statement
exec :: Statement -> ExecMonad
exec e = do
  returnWasCalled <- getControlValue ReturnParameter
  breakWasCalled <- getControlValue BreakParameter
  continueWasCalled <- getControlValue ContinueParameter
  s <- gets controlValues
  let execute = not (returnWasCalled || breakWasCalled || continueWasCalled)
  if execute then internalExec e else return ()

internalExec :: Statement -> ExecMonad
internalExec (Empty _) = return ()

internalExec (Sequence []) = return ()
internalExec (Sequence ((VarDecl _ varType ds):rest)) = declareVariablesAndExecuteRest varType ds (Sequence rest)
internalExec (Sequence ((FnDef cur fnType (Ident fnName) args (Block _ body)):rest)) =
  declareFunctionAndExecuteRest fnType fnName cur args (Sequence body) (Sequence rest)
internalExec (Sequence (i:is)) = do
  exec i
  exec (Sequence is)

internalExec (Ret _ e) = do
  v <- eval e
  setControlValue ReturnParameter (ReturnValue (Just v))
internalExec (VRet _) = setControlValue ReturnParameter (ReturnValue (Just VoidValue))
internalExec (Break _) = setControlValue BreakParameter (Flag True)
internalExec (Continue _) = setControlValue ContinueParameter (Flag True)

internalExec d@VarDecl{} = exec (Sequence [d])  -- no instructions are after this variable declaration
internalExec d@FnDef{} = exec (Sequence [d])  -- no instructions are after this function declaration

internalExec (Assign cur (Ident x) e) = do
  s <- gets values
  r <- asks variables
  v <- eval e
  let optLoc = Map.lookup x r
  case optLoc of
    Just l -> modify (modifyVariableStore $ Map.insert l v s)
    Nothing -> throwError $ undeclaredVariable x cur
internalExec (IndAssign cur (Ident a) index e) = do
  valueToAssign <- eval e
  i <- eval index
  arr <- eval (EVar cur (Ident a))
  case (i, arr) of
    (VInt v, VArray t vec) -> do
      let newVec = Vector.update vec (Vector.singleton (fromInteger v, valueToAssign))
      assignValueToVariable (VArray t newVec) a cur
    (_, VArray{}) -> throwError $ TypeError "array index must be an integer" cur
    _ -> throwError $ TypeError (a ++ " is not an array") cur
internalExec (Incr cur ident) = exec (Assign cur ident (EAdd cur (EVar cur ident) (Plus cur) (ELitInt cur 1)))
internalExec (Decr cur ident) = exec (Assign cur ident (EAdd cur (EVar cur ident) (Minus cur) (ELitInt cur 1)))

internalExec (Print _ e) = processExpValue e printValue where
  printValue (VString s) = liftIO $ putStrLn s
  printValue v = liftIO $ putStrLn $ show v
internalExec (SExp _ e) = processExpValue e (const $ return ())


{---------------------------------------------------------
                    CONDITIONALS AND LOOPS
----------------------------------------------------------}
internalExec (CondElse cur e caseTrue caseFalse) = do
  b <- eval e
  case b of
    (VBool v) -> execBlock $ if v then caseTrue else caseFalse
    other -> throwError $ TypeError ("incompatible type in condition " ++
                                     show other) cur
internalExec (Cond cur e bl) = exec (CondElse cur e bl emptyBlock)

internalExec (While _ e body) = do
  s <- gets controlValues
  r <- ask
  let
    l = alloc s
    generateSingleLocation val (locs, s') = let l = alloc s' in (l:locs, Map.insert l val s')
    -- initially set break and continue flags to false
    ([breakLoc, continueLoc], s') = foldr generateSingleLocation ([], s) [Flag False, Flag False]
    newEnv = declareControlValues [BreakParameter, ContinueParameter] [breakLoc, continueLoc] r
  modify (modifyControlStore s')
  whileLoop e body newEnv breakLoc continueLoc

whileLoop e body whileEnv breakLoc continueLoc = do
  b <- eval e
  case b of
    (VBool v) ->
      if v
        -- execute loop body
        then do
          local (const whileEnv) (execBlock body)
          -- check if break or continue was called and handle them if so
          s <- gets controlValues
          case (Map.lookup breakLoc s, Map.lookup continueLoc s) of
            -- break was called
            (Just (Flag True), _) -> return ()
            -- continue was called
            (_, Just (Flag True)) -> do
              modify (modifyControlStore $ Map.insert continueLoc (Flag False) s)
              whileLoop e body whileEnv breakLoc continueLoc
            -- simply execute the next iteration of the loop
            _ -> whileLoop e body whileEnv breakLoc continueLoc
        -- don't execute loop body
        else return ()
    other -> throwError $ TypeError ("incompatible type in condition " ++
                                      show other) (getCursor e)


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
declareFunctionAndExecuteRest :: Type Cursor -> FunctionName -> Cursor -> [Arg Cursor] -> Statement -> Statement -> ExecMonad
declareFunctionAndExecuteRest fnType fnName cur formalArgs body rest = do
  rOfDeclaration <- ask
  let
    f :: Function
    f actualArgs = do
      -- prepare environment for body execution
      variableEnvOfApplication <- asks variables
      (env, retLocation) <- getFunctionEnvironmentAndReturnLocation fnName f variableEnvOfApplication rOfDeclaration formalArgs actualArgs
      -- execute body
      local (const env) (exec body)
      -- process return value
      s <- gets controlValues
      case (Map.lookup retLocation s, fnType) of
        (Just (ReturnValue (Just v)), _) -> return v
        (_, Void _) -> return VoidValue
        _ -> throwError $ TypeError "function didn't return anything" cur
  local (declareFunction fnName f) (exec rest)

getFunctionEnvironmentAndReturnLocation :: FunctionName -> Function -> VariableEnvironment -> Environment ->
  [Arg Cursor] -> [Expression] -> InterpreterMonad (Environment, Location)
getFunctionEnvironmentAndReturnLocation fnName f variableEnvOfApplication rOfDeclaration formalArgs actualArgs = do
  s <- gets controlValues
  argumentsDeclaredEnv <- getEnvironmentWithActualArgs formalArgs actualArgs variableEnvOfApplication rOfDeclaration
  let
    resultEnvironment = declareControlValue ReturnParameter retLocation functionAndArgumentsDeclaredEnv
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
    generateSingleLocation (ArgRef{}, e) _ = throwError $ TypeError "argument passed by reference must be a variable" (getCursor e)
  (argLocations, s') <- foldrM generateSingleLocation ([], s) (zip formalArgs actualArgs)
  modify (modifyVariableStore s')
  return $ declareVariables argNames argLocations rOfDeclaration


{---------------------------------------------------------
                    GENERAL HELPER FUNCTIONS
----------------------------------------------------------}
assignValueToVariable :: Value -> VariableName -> Cursor -> ExecMonad
assignValueToVariable v x cur = do
  s <- gets values
  r <- asks variables
  let optLoc = Map.lookup x r
  case optLoc of
    Just l -> modify (modifyVariableStore $ Map.insert l v s)
    Nothing -> throwError $ undeclaredVariable x cur

getControlValue :: ControlParameter -> InterpreterMonad Bool
getControlValue x = do
  s <- gets controlValues
  r <- asks control
  return $ case do
    l <- Map.lookup x r
    Map.lookup l s of
      Just (ReturnValue (Just _)) -> True
      Just (Flag b) -> b
      _ -> False

setControlValue :: ControlParameter -> ControlValue -> ExecMonad
setControlValue parameter val = do
  s <- gets controlValues
  r <- asks control
  let loc = Map.lookup parameter r
  case loc of
    Just l -> modify (modifyControlStore $ Map.insert l val s)
    -- _ -> return () -- TODO
    _ -> error "not found"
