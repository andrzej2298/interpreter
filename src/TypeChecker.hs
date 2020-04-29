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
type TypeCheckMonad = TypeCheckMonadT Statement
type TypeCheckResult = Either Error CursorProgram

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

infixr 5 :->
data FunctionType = [TypeValue] :-> TypeValue

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

itemType :: Item Cursor -> TypeValue -> TypeCheckMonadT (Cursor, TypeValue, VariableName)
itemType (NoInit cur (Ident x)) declaredType = return (cur, declaredType, x)
itemType (Init cur (Ident x) e) _ = do
  t <- typeOf e
  return (cur, t, x)


check :: Statement -> TypeCheckMonad
check (Sequence (i:is)) = do
  ci <- check i
  (Sequence cis) <- check (Sequence is)
  return (Sequence (ci:cis))
check i@(VarDecl _ t e) = do
  tv <- getTypeFromSyntax t
  itemTypes <- mapM (flip itemType tv) e
  let
    checkAll :: [(Cursor, TypeValue, VariableName)] -> TypeValue -> TypeCheckMonadT ()
    checkAll [] _ = return ()
    checkAll ((cur, actualType, _):l) expectedType
      | actualType == expectedType = checkAll l expectedType
      | otherwise = throwError $ TypeError (concat ["type mismatch: expected ", show expectedType,
                                                    ", but got: ", show actualType]) cur
  checkAll itemTypes tv
  return i
check i = return i

checkTypes :: CursorProgram -> IO TypeCheckResult
checkTypes (Program cur p) = do
  let
    r = Map.empty
    s = Map.empty
  result <- evalStateT (runExceptT (runReaderT (check (Sequence p)) r)) s
  case result of
    Left l -> return $ Left l  -- these two Lefts have different types
    Right (Sequence is) -> return $ Right $ Program cur is
