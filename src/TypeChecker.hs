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
  functionTypes :: FunctionTypeEnvironment,
  returnType :: ReturnTypeLocation
}

type VariableTypeEnvironment = Map.Map VariableName Location
type FunctionTypeEnvironment = Map.Map FunctionName FunctionType
type ReturnTypeLocation = Maybe Location

type VariableTypeStore = Map.Map Location TypeValue
type ReturnTypeStore = Map.Map Location TypeValue

data TypeStore = TypeStore {
  variableTypeValues :: VariableTypeStore,
  returnTypeValues :: ReturnTypeStore
}

data TypeValue
  = TInt
  | TStr
  | TBool
  | TVoid
  | TArray TypeValue
  | TTuple (Vector.Vector TypeValue)
  deriving (Eq)


-- top level function which checks the types
checkTypes :: CursorProgram -> IO TypeCheckResult
checkTypes (Program _ p) = do
  let
    r = TypeEnvironment {
      variableTypes = Map.empty,
      functionTypes = Map.empty,
      returnType = Nothing
    }
    s = TypeStore {
      variableTypeValues = Map.empty,
      returnTypeValues = Map.empty
    }
  result <- evalStateT (runExceptT (runReaderT (check (Sequence p)) r)) s
  case result of
    Left l -> return $ Left l  -- these two Lefts have different types
    Right () -> return $ Right ()

{---------------------------------------------------------
                    EXPRESSION TYPES
----------------------------------------------------------}
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
    (f1, f2, Minus _) -> throwError $ TypeError ("can't subtract " ++ show f2 ++ " from " ++ show f1) cur
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
typeOf (EApp cur (Ident "len") actualArgs) = do
  -- len is special, because it can take arrays of different types
  -- as input
  when
    (length actualArgs /= 1)
    (throwError $ TypeError
     "wrong number of arguments: expected 1" cur)
  [t] <- mapM typeOf actualArgs
  unless
    (isArray t)
    (throwError $ TypeError "can only check length of a single array" cur)
  return TInt
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
    matchOneType (ByReference _, e) = throwError $ TypeError "argument passed by reference must be a variable" (getCursor e)
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
    _ -> throwError $ TypeError "expected tuple or array" cur

getVariableType :: VariableName -> Cursor -> TypeCheckMonadT TypeValue
getVariableType x cur = do
  r <- asks variableTypes
  s <- gets variableTypeValues
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
      Nothing -> throwError $ undeclaredFunction fn cur

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

{---------------------------------------------------------
                    STATEMENT CORRECTNESS
----------------------------------------------------------}
check :: Statement -> TypeCheckMonad
check Empty{} = return ()
check (Sequence ((VarDecl cur t e):rest)) = do
  expected <- getTypeFromSyntax t
  case expected of
    TVoid -> throwError $ TypeError "variable can't be void" cur
    _ -> return ()
  -- check if assigned expressions match the expected type
  itemSignatures <- mapM (itemSignature expected) e
  let
    variableNames = map (\(_, _, x) -> x) itemSignatures
    checkAll :: [(Cursor, TypeValue, VariableName)] -> TypeValue -> TypeCheckMonad
    checkAll [] _ = return ()
    checkAll ((varCur, actualType, _):l) expectedType
      | actualType == expectedType = checkAll l expectedType
      | otherwise = throwError $ typeMismatchError expectedType actualType varCur
  checkAll itemSignatures expected
  -- declare the variables
  s <- gets variableTypeValues
  let
    types = replicate (length itemSignatures) expected
    (locs, s') = generateLocationsForValues s types
  modify (modifyVariableTypeStore s')
  local (declareVariableTypes variableNames locs) (check (Sequence rest))

check (Sequence ((FnDef cur fnType (Ident fnName) args body):rest)) = do
  s <- gets variableTypeValues
  retStore <- gets returnTypeValues
  fnReturnType <- getTypeFromSyntax fnType
  formalArgSignatures <- mapM getFunctionArgument args
  -- check the body of the function with args and function defined
  let
    formalArgTypes = map snd formalArgSignatures
    formalTypes = map getTypeFromArgType formalArgTypes
    formalArgNames = map fst formalArgSignatures
    f :: FunctionType
    f = formalArgTypes :-> fnReturnType
    (locs, s') = generateLocationsForValues s formalTypes
    ([retLoc], retStore') = generateLocationsForValues retStore [fnReturnType]
    envTransformFunction = declareFunctionType fnName f .
                           declareVariableTypes formalArgNames locs
  modify (modifyReturnLocationStore retStore' . modifyVariableTypeStore s')
  local (declareReturnType retLoc . envTransformFunction) (checkBlock body)
  -- check if the function returned (or is void)
  returnPresent <- checkReturnPresentBlock body
  unless
    (returnPresent || isVoid fnReturnType)
    (throwError $ TypeError ("there exists a path with no return in function " ++ fnName) cur)
  -- check rest in an environment with the function
  local envTransformFunction (check (Sequence rest))
check (Ret cur e) = do
  actualReturnType <- typeOf e
  checkReturn cur actualReturnType
check (VRet cur) = checkReturn cur TVoid
check (Sequence (i:is)) = do
  check i
  check (Sequence is)
check (Sequence []) = return ()
check v@VarDecl{} = check (Sequence [v])
check f@FnDef{} = check (Sequence [f])
check (IndAssign cur (Ident a) index e) = do
  aType <- getVariableType a cur
  -- tuples are immutable
  case aType of
    TTuple{} -> throwError $ TypeError "tuples are immutable" cur
    _ -> return ()
  expected <- typeOf (EItemInd cur (Ident a) index)
  actual <- typeOf e
  when
    (expected /= actual)
    (throwError $ typeMismatchError expected actual cur)
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
check (Print cur e) = do
  t <- typeOf e
  case t of
    TVoid -> throwError $ TypeError "can't print void" cur
    _ -> return ()


-- check if value returned by function is same as expected
checkReturn :: Cursor -> TypeValue -> TypeCheckMonad
checkReturn cur actualReturnType = do
  loc <- asks returnType
  s <- gets returnTypeValues
  case do
    l <- loc
    Map.lookup l s of
      Just expectedReturnType -> when
        (expectedReturnType /= actualReturnType)
        (throwError $ typeMismatchError expectedReturnType actualReturnType cur)
      Nothing -> throwError $ TypeError "return outside of a function" cur


checkReturnPresentBlock :: Block Cursor -> TypeCheckMonadT Bool
checkReturnPresentBlock (Block _ is) = checkReturnPresent (Sequence is)

checkReturnPresent :: Statement -> TypeCheckMonadT Bool
checkReturnPresent (Ret _ _) = return True
checkReturnPresent (Sequence is) = do
  rets <- mapM checkReturnPresent is
  return $ or rets  -- some statement in sequence must return
checkReturnPresent (CondElse _ _ caseTrue caseFalse) = do
  truePath <- checkReturnPresentBlock caseTrue
  falsePath <- checkReturnPresentBlock caseFalse
  return $ truePath && falsePath
checkReturnPresent _ = return False


{---------------------------------------------------------
                    HELPER FUNCTIONS
----------------------------------------------------------}
declareVariableTypes :: [VariableName] -> [Location] -> TypeEnvironment -> TypeEnvironment
declareVariableTypes xs ls env = env { variableTypes = insertManyValues xs ls (variableTypes env) }

declareFunctionType :: FunctionName -> FunctionType -> TypeEnvironment -> TypeEnvironment
declareFunctionType f t env = env { functionTypes = Map.insert f t (functionTypes env) }

declareReturnType :: Location -> TypeEnvironment -> TypeEnvironment
declareReturnType l env = env { returnType = Just l }

modifyVariableTypeStore :: VariableTypeStore -> TypeStore -> TypeStore
modifyVariableTypeStore s env = env { variableTypeValues = s }

modifyReturnLocationStore :: ReturnTypeStore -> TypeStore -> TypeStore
modifyReturnLocationStore s env = env { returnTypeValues = s }

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
