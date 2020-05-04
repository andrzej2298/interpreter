module TypeChecker (checkTypes) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import AbsGrammar
import CommonDeclarations

type TypeCheckMonadT a = ReaderT TypeEnvironment (ExceptT Error
                                                  (StateT TypeStore IO)) a
type TypeCheckMonad = TypeCheckMonadT ()
type TypeCheckResult = Either Error ()

infixr 5 :->
data FunctionArgument = ByValue TypeValue | ByReference TypeValue
  deriving (Show)
data FunctionType = [FunctionArgument] :-> TypeValue

data TypeEnvironment = TypeEnvironment {
  variableTypes :: VariableTypeEnvironment,
  functionTypes :: FunctionTypeEnvironment
}

type VariableTypeEnvironment = Map.Map VariableName Location
type FunctionTypeEnvironment = Map.Map FunctionName FunctionType
type TypeStore = Map.Map Location TypeValue

data TypeValue
  = TInt
  | TStr
  | TBool
  | TVoid
  | TArray TypeValue
  | TTuple (Vector.Vector TypeValue)
  deriving (Eq)

declareVariableTypes :: [VariableName] -> [Location] -> TypeEnvironment -> TypeEnvironment
declareVariableTypes xs ls env = env { variableTypes = insertManyValues xs ls (variableTypes env) }

declareFunctionType :: FunctionName -> FunctionType -> TypeEnvironment -> TypeEnvironment
declareFunctionType f t env = env { functionTypes = Map.insert f t (functionTypes env) }

typeMismatchError :: TypeValue -> TypeValue -> Cursor -> Error
typeMismatchError expectedType actualType =
  TypeError (concat ["type mismatch: expected ", show expectedType, ", but got ", show actualType])

instance Show TypeValue where
  show t = showsType t "" where
    showsType TInt = showString "int"
    showsType TStr = showString "string"
    showsType TBool = showString "bool"
    showsType TVoid = showString "void"
    showsType (TArray at) = shows at . showString "[]"
    showsType (TTuple ts) = showString "<" . showString (intercalate ", " $ map show (Vector.toList ts)) . showString ">"

isSimple :: TypeValue -> Bool
isSimple TArray{} = False
isSimple TTuple{} = False
isSimple _ = True

isArray :: TypeValue -> Bool
isArray TArray{} = True
isArray _ = False

isVoid :: TypeValue -> Bool
isVoid TVoid = True
isVoid _ = False

getTypeFromSyntax :: Type Cursor -> TypeCheckMonadT TypeValue
getTypeFromSyntax (Int _) = return TInt
getTypeFromSyntax (Str _) = return TStr
getTypeFromSyntax (Bool _) = return TBool
getTypeFromSyntax (Void _) = return TVoid
getTypeFromSyntax (Array cur t) = do
  vt <- getTypeFromSyntax t
  let err = TypeError "array must be of a simple type" cur
  if isSimple vt then return $ TArray vt else throwError err
getTypeFromSyntax (Tuple cur ts) = do
  vts <- mapM getTypeFromSyntax ts
  let err = TypeError "tuple can't hold an array" cur
  if any isArray vts then throwError err else return $ TTuple (Vector.fromList vts)

typeOf :: Expression -> TypeCheckMonadT TypeValue
typeOf (ELitInt _ _) = return TInt
typeOf (ELitTrue _) = return TBool
typeOf (ELitFalse _) = return TBool
typeOf (EString _ _) = return TStr
typeOf (Not cur e) = do
  t <- typeOf e
  case t of
    TBool -> return TBool
    v -> throwError $ TypeError ("can't negate value of type " ++ show v) cur
typeOf (Neg cur e) = do
  t <- typeOf e
  case t of
    TInt -> return TInt
    v -> throwError $ TypeError ("can't negate value of type " ++ show v) cur
typeOf (ETupLit cur ids) = do
  vts <- mapM typeOf ids
  let err = TypeError "tuple can't hold an array" cur
  if any isArray vts then throwError err else return $ TTuple (Vector.fromList vts)
typeOf (EArrDef cur t e) = do
  tv <- getTypeFromSyntax t
  n <- typeOf e
  case n of
    TInt -> return $ TArray tv
    _ -> throwError $ TypeError "array size must be an integer" cur
typeOf (EArrExp cur []) = throwError $ TypeError "explicitly defined array can't be empty" cur
typeOf (EArrExp cur exprs) = do
  types <- mapM typeOf exprs
  let
    t = head types
    typeMismatch = any (/= t) types
  if typeMismatch
    then throwError $ TypeError "all elements in the array must be the same type" cur
    else return $ TArray t
typeOf (EAdd cur e1 op e2) = do
  t1 <- typeOf e1
  t2 <- typeOf e2
  case (t1, t2, op) of
    (TInt, TInt, _) -> return TInt
    (TStr, TStr, Plus _) -> return TStr
    (f1, f2, Plus _) -> throwError $ TypeError ("can't add " ++ show f1 ++ " to " ++ show f2) cur
    (f1, f2, Minus _) -> throwError $ TypeError ("can't subtract " ++ show f1 ++ " from " ++ show f2) cur
typeOf (EMul cur e1 _ e2) = do
  t1 <- typeOf e1
  t2 <- typeOf e2
  case (t1, t2) of
    (TInt, TInt) -> return TInt
    (f1, f2) -> throwError $ TypeError ("can't multiply or divide " ++ show f1 ++ " and " ++ show f2) cur
typeOf (ERel cur e1 op e2) = do
  t1 <- typeOf e1
  t2 <- typeOf e2
  let
    err t u = TypeError ("can't compare " ++ show t ++ " and " ++ show u) cur
    checkEqual :: TypeValue -> TypeValue -> TypeCheckMonadT TypeValue
    checkEqual t u
      | t == u = return TBool
      | otherwise = throwError $ err t u
  case (t1, t2, op) of
    (TInt, TInt, _) -> return TBool
    (_, _, EQU _) -> checkEqual t1 t2
    (_, _, NEQ _) -> checkEqual t1 t2
    _ -> throwError $ err t1 t2
typeOf (EAnd cur e1 e2) = typeOfLogicOp cur e1 e2
typeOf (EOr cur e1 e2) = typeOfLogicOp cur e1 e2
typeOf (EVar cur (Ident x)) = getVariableType x cur
typeOf (EApp cur (Ident fn) actualArgs) = do
  (formalArgs :-> resultType) <- getFunctionType fn cur
  when
    (length formalArgs /= length actualArgs)
    (throwError $ TypeError
     ("wrong number of arguments: expected " ++ show (length formalArgs)) cur)
  let
    checkArgumentType :: TypeValue -> Expression -> TypeCheckMonad
    checkArgumentType expected expr = do
      actual <- typeOf expr
      when (actual /= expected) (throwError $ typeMismatchError expected actual (getCursor expr))
    matchOneType :: (FunctionArgument, Expression) -> TypeCheckMonad
    matchOneType (ByValue expected, expr) = checkArgumentType expected expr
    matchOneType (ByReference expected, expr@EVar{}) = checkArgumentType expected expr
    matchOneType (ByReference t, e) = throwError $ TypeError "argument passed by reference must be a variable" (getCursor e)
  mapM_ matchOneType (zip formalArgs actualArgs)
  return resultType

typeOf (EItemInd cur (Ident x) e) = do
  indexT <- typeOf e
  varT <- getVariableType x cur
  case (indexT, varT, e) of
    (TInt, TArray arrT, _) -> return arrT
    (_, TTuple types, ELitInt _ n) -> case types Vector.!? fromInteger n of
      Just tupleT -> return tupleT
      Nothing -> throwError $ TypeError "tuple index out of bounds" cur
    (_, TArray{}, _) -> throwError $ TypeError "array index must be an integer" cur
    (_, TTuple{}, _) -> throwError $ TypeError "tuple index must be a positive static integer" cur
typeOf e = throwError $ TypeError (show e ++ " not implemented") (getCursor e)

getVariableType :: VariableName -> Cursor -> TypeCheckMonadT TypeValue
getVariableType x cur = do
  r <- asks variableTypes
  s <- get
  case do
    l <- Map.lookup x r
    Map.lookup l s of
      Just t -> return t
      Nothing -> throwError $ undeclaredVariable x cur

getFunctionType :: FunctionName -> Cursor -> TypeCheckMonadT FunctionType
getFunctionType fn cur = do
  r <- asks functionTypes
  case Map.lookup fn r of
      Just t -> return t
      Nothing -> throwError $ undeclaredVariable fn cur

getFunctionArgument :: Arg Cursor -> TypeCheckMonadT (VariableName, FunctionArgument)
getFunctionArgument (ArgVal _ t (Ident x)) = do
  argT <- getTypeFromSyntax t
  return (x, ByValue argT)
getFunctionArgument (ArgRef _ t (Ident x)) = do
  argT <- getTypeFromSyntax t
  return (x, ByReference argT)

getTypeFromArgType :: FunctionArgument -> TypeValue
getTypeFromArgType (ByValue t) = t
getTypeFromArgType (ByReference t) = t

typeOfLogicOp :: Cursor -> Expression -> Expression -> TypeCheckMonadT TypeValue
typeOfLogicOp cur e1 e2 = do
  t1 <- typeOf e1
  t2 <- typeOf e2
  case (t1, t2) of
    (TBool, TBool) -> return TBool
    _ -> throwError $ TypeError ("can't perform && or || of " ++
                                 show t1 ++ " and " ++ show t2) cur

itemSignature :: TypeValue -> Item Cursor -> TypeCheckMonadT (Cursor, TypeValue, VariableName)
itemSignature declaredType (NoInit cur (Ident x)) = return (cur, declaredType, x)
itemSignature _ (Init cur (Ident x) e) = do
  t <- typeOf e
  return (cur, t, x)

checkIncrDecr :: Cursor -> VariableName -> TypeCheckMonad
checkIncrDecr cur x = do
  actual <- getVariableType x cur
  if actual == TInt then return () else throwError $ typeMismatchError TInt actual cur

checkBlock :: Block Cursor -> TypeCheckMonad
checkBlock (Block _ is) = check (Sequence is)

check :: Statement -> TypeCheckMonad
check Empty{} = return ()
check (Sequence ((VarDecl cur t e):rest)) = do
  expected <- getTypeFromSyntax t
  case expected of
    TVoid -> throwError $ TypeError "variable can't be void" cur
    _ -> return ()
  itemSignatures <- mapM (itemSignature expected) e
  let
    variableNames = map (\(_, _, x) -> x) itemSignatures
    checkAll :: [(Cursor, TypeValue, VariableName)] -> TypeValue -> TypeCheckMonad
    checkAll [] _ = return ()
    checkAll ((varCur, actualType, _):l) expectedType
      | actualType == expectedType = checkAll l expectedType
      | otherwise = throwError $ typeMismatchError expectedType actualType varCur
  checkAll itemSignatures expected
  s <- get
  let
    types = replicate (length itemSignatures) expected
    (locs, s') = generateLocationsForValues s types
  put s'
  local (declareVariableTypes variableNames locs) (check (Sequence rest))
check (Sequence ((FnDef cur fnType (Ident fnName) args body):rest)) = do
  envOfDeclaration <- ask
  s <- get
  returnType <- getTypeFromSyntax fnType
  formalArgSignatures <- mapM getFunctionArgument args
  let
    formalArgTypes = map snd formalArgSignatures
    formalTypes = map getTypeFromArgType formalArgTypes
    formalArgNames = map fst formalArgSignatures
    f :: FunctionType
    f = formalArgTypes :-> returnType
    (locs, s') = generateLocationsForValues s formalTypes
    envTransformFunction = declareFunctionType fnName f .
                           declareVariableTypes formalArgNames locs
  put s'
  returnPresent <- checkReturnPresentBlock body
  unless
    (returnPresent || isVoid returnType)
    (throwError $ TypeError ("there exists a path with no return in function " ++ fnName) cur)
  local envTransformFunction (checkBlock body)
  local envTransformFunction (check (Sequence rest))
check (Ret _ _) = return ()  -- TODO
check (VRet _) = return ()  -- TODO
check (Sequence (i:is)) = do
  check i
  check (Sequence is)
check (Sequence []) = return ()
check v@VarDecl{} = check (Sequence [v])
check f@FnDef{} = check (Sequence [f])
check (Assign cur (Ident x) e) = do
  actual <- typeOf e
  expected <- getVariableType x cur
  when (actual /= expected) (throwError $ typeMismatchError expected actual cur)
check (Incr cur (Ident x)) = checkIncrDecr cur x
check (Decr cur (Ident x)) = checkIncrDecr cur x
check (TupTie cur tievars e) = do
  exprT <- typeOf e
  let
    lengthError = TypeError "variables don't match the tuple structure"
    typeError = TypeError "can tie only a tuple"
    checkOne :: (TieVar Cursor, TypeValue) -> TypeCheckMonad
    checkOne (TieIgnore _, _) = return ()
    checkOne (TieVar tCur (Ident x), t) = do
      expected <- getVariableType x tCur
      if expected == t then return () else throwError $ typeError tCur
    checkOne (TieVars tCur vars, t) = case t of
      (TTuple types) -> if length types == length vars
        then mapM_ checkOne (zip vars (Vector.toList types))
        else throwError $ lengthError cur
      _ -> throwError $ typeError tCur
  case exprT of
    TTuple types -> if length types == length tievars
      then mapM_ checkOne (zip tievars (Vector.toList types))
      else throwError $ lengthError cur
    _ -> throwError $ typeError cur
check (While cur e body) = do
  condT <- typeOf e
  case condT of
    TBool -> checkBlock body
    _ -> throwError $ TypeError "condition must be of type bool" cur
check Break{} = return ()
check Continue{} = return ()
check (SExp _ e) = do
  _ <- typeOf e  -- check if expression is correct
  return ()
check (CondElse cur e caseTrue caseFalse) = do
  b <- typeOf e
  case b of
    TBool -> do
      checkBlock caseTrue
      checkBlock caseFalse
    _ -> throwError $ TypeError "condition must be of type bool" cur
check (Cond cur e bl) = check (CondElse cur e bl emptyBlock)
check (Print _ e) = do
  _ <- typeOf e
  return ()
check i = throwError $ TypeError (show i) (0, 0)  -- TODO temporary
-- check i = return ()  -- TODO temporary

checkTypes :: CursorProgram -> IO TypeCheckResult
-- checkTypes (Program _ p) = return $ Right ()
checkTypes (Program _ p) = do
  let
    r = TypeEnvironment { variableTypes = Map.empty, functionTypes = Map.empty }
    s = Map.empty
  result <- evalStateT (runExceptT (runReaderT (check (Sequence p)) r)) s
  case result of
    Left l -> return $ Left l  -- these two Lefts have different types
    Right () -> return $ Right ()


checkReturnPresentBlock :: Block Cursor -> TypeCheckMonadT Bool
checkReturnPresentBlock (Block _ is) = checkReturnPresent (Sequence is)

checkReturnPresent :: Statement -> TypeCheckMonadT Bool
checkReturnPresent (Ret _ _) = return True
checkReturnPresent (Sequence is) = do
  rets <- mapM checkReturnPresent is
  liftIO $ print is
  liftIO $ print rets
  return $ or rets  -- any statement in sequence must return
checkReturnPresent (CondElse cur _ caseTrue caseFalse) = do
  truePath <- checkReturnPresentBlock caseTrue
  falsePath <- checkReturnPresentBlock caseFalse
  return $ truePath && falsePath
checkReturnPresent _ = return False