module TypeChecker (checkTypes) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import qualified Data.Map as Map

import AbsGrammar
import CommonDeclarations

type TypeCheckMonadT a = ReaderT TypeEnvironment (ExceptT Error
                                                  (StateT TypeStore IO)) a
type TypeCheckMonad = TypeCheckMonadT ()
type TypeCheckResult = Either Error ()

type TypeEnvironment = Map.Map VariableName Location
type TypeStore = Map.Map Location TypeValue

data TypeValue
  = TInt
  | TStr
  | TBool
  | TVoid
  | TArray TypeValue
  | TTuple [TypeValue]
  deriving (Eq)

typeMismatchError :: TypeValue -> TypeValue -> Cursor -> Error
typeMismatchError expectedType actualType =
  TypeError (concat ["type mismatch: expected ", show expectedType, ", but got: ", show actualType])

-- infixr 5 :->
-- data FunctionArgument = ByValue TypeValue
-- data FunctionType = [TypeValue] :-> TypeValue

instance Show TypeValue where
  show t = showsType t "" where
    showsType TInt = showString "int"
    showsType TStr = showString "string"
    showsType TBool = showString "bool"
    showsType TVoid = showString "void"
    showsType (TArray at) = shows at . showString "[]"
    showsType (TTuple ts) = showString "<" . showString (intercalate ", " $ map show ts) . showString ">"

isSimple :: TypeValue -> Bool
isSimple TArray{} = False
isSimple TTuple{} = False
isSimple _ = True

isArray :: TypeValue -> Bool
isArray TArray{} = True
isArray _ = False

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
  if any isArray vts then throwError err else return $ TTuple vts

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
  if any isArray vts then throwError err else return $ TTuple vts
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


checkBlock :: Block Cursor -> TypeCheckMonadT ()
checkBlock (Block _ is) = check (Sequence is)

check :: Statement -> TypeCheckMonad
-- internalExec (Sequence []) = return ()
-- internalExec (Sequence ((VarDecl _ varType ds):rest)) = declareVariablesAndExecuteRest varType ds (Sequence rest)
-- internalExec (Sequence ((FnDef cur fnType (Ident fnName) args (Block _ body)):rest)) =
--   declareFunctionAndExecuteRest fnType fnName cur args (Sequence body) (Sequence rest)
-- internalExec (Sequence (i:is)) = do
--   exec i
--   exec (Sequence is)
check Empty{} = return ()
--   t <- typeOf e
--   r <- ask
--   s <- get
--   let
--     l = alloc s
--     s' = Map.insert l t s
--   put s'
check (Sequence ((VarDecl cur t e):rest)) = do
  expected <- getTypeFromSyntax t
  case expected of
    TVoid -> throwError $ TypeError "variable can't be void" cur
    _ -> return ()
  itemSignatures <- mapM (itemSignature expected) e
  let
    variableNames = map (\(_, _, x) -> x) itemSignatures
    checkAll :: [(Cursor, TypeValue, VariableName)] -> TypeValue -> TypeCheckMonadT ()
    checkAll [] _ = return ()
    checkAll ((varCur, actualType, _):l) expectedType
      | actualType == expectedType = checkAll l expectedType
      | otherwise = throwError $ typeMismatchError expectedType actualType varCur
  checkAll itemSignatures expected
  r <- ask
  s <- get
  let
    types = replicate (length itemSignatures) expected
    (locs, s') = generateLocationsForValues s types
  put s'
  local (insertManyValues variableNames locs) (check (Sequence rest))
check (Sequence (i:is)) = do
  check i
  check (Sequence is)
check (Sequence []) = return ()
check (Assign cur (Ident x) e) = do
  r <- ask
  s <- get
  actual <- typeOf e
  case do
    l <- Map.lookup x r
    Map.lookup l s of
      Just expected -> if actual == expected
        then return ()
        else throwError $ typeMismatchError expected actual cur
      Nothing -> throwError $ undeclaredVariable x cur
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

checkTypes :: CursorProgram -> IO TypeCheckResult
checkTypes (Program _ p) = do
  let
    r = Map.empty
    s = Map.empty
  result <- evalStateT (runExceptT (runReaderT (check (Sequence p)) r)) s
  case result of
    Left l -> return $ Left l  -- these two Lefts have different types
    Right () -> return $ Right ()
