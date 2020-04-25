module CommonDeclarations where

import System.IO (hPutStrLn, stderr)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import Data.List
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import AbsGrammar

type Cursor = (Int, Int)
type Expression = Expr Cursor
type Statement = Stmt Cursor
type CursorProgram = Program Cursor

type VariableName = String
type FunctionName = String
data ControlParameter = Return | Break | Continue
  deriving (Eq, Ord, Show)
type Location = Int

data Value = VInt Integer | VBool Bool | VString String | VoidValue | VArray (Type Cursor) (Vector.Vector Value)
type Function = [Expression] -> EvalMonad
data ControlValue = ReturnValue (Maybe Value) | Flag Bool

dummyCursor = (0, 0)
getType :: Value -> Type Cursor
getType (VInt _) = Int dummyCursor
getType (VBool _) = Bool dummyCursor
getType (VString _) = Str dummyCursor
getType (VArray t _) = Array dummyCursor t

defaultValue :: Type a -> Value
defaultValue (Int _) = VInt 0
defaultValue (Str _) = VString ""
defaultValue (Bool _) = VBool False

instance Show Value where
  show v = showsValue v "" where
    showsValue (VInt i) = shows i
    showsValue (VBool b) = showString $ map toLower (show b)
    showsValue (VString s) = showString "\"" . showString s . showString "\""
    showsValue (VArray _ vec) = showString "[" . showString (intercalate ", " $ map show (Vector.toList vec)) . showString "]"

showValueType :: Value -> String
showValueType = show . getType
  -- show (VInt _) = "Integer"
  -- show (VBool _) = "Bool"
  -- show (VString _) = "String"

data Error =
  ArithmeticError String Cursor |
  TypeError String Cursor |
  NameError String Cursor |
  RuntimeError String Cursor

data Environment = Environment {
  functions :: FunctionEnvironment,
  variables :: VariableEnvironment,
  control :: ControlEnvironment
}
instance Show Environment where
  show e = show (Map.keys $ functions e) ++ " " ++ show (Map.keys $ variables e) ++
    " " ++ show (Map.keys $ control e)
type VariableEnvironment = Map.Map VariableName Location
type FunctionEnvironment = Map.Map FunctionName Function
type ControlEnvironment = Map.Map ControlParameter Location

data Store = Store {
  values :: VariableStore,
  controlValues :: ControlStore
}

type VariableStore = Map.Map Location Value
type ControlStore = Map.Map Location ControlValue

emptyEnvironment :: Environment
emptyEnvironment = Environment { functions = Map.empty, variables = Map.empty, control = Map.empty }
declareVariable :: VariableName -> Location -> Environment -> Environment
declareVariable x l env = env { variables = Map.insert x l (variables env) }
declareVariables :: [VariableName] -> [Location] -> Environment -> Environment
declareVariables xs ls env = env { variables = insertManyValues xs ls (variables env) }
declareFunction :: FunctionName -> Function -> Environment -> Environment
declareFunction fn f env = env { functions = Map.insert fn f (functions env) }
declareReturnValue :: Location -> Environment -> Environment
declareReturnValue l env = env { control = Map.insert Return l (control env) }

emptyStore :: Store
emptyStore = Store {
  values = Map.empty,
  controlValues = Map.empty
}
modifyVariableStore :: VariableStore -> Store -> Store
modifyVariableStore s' st = st { values = s' }
modifyControlStore :: ControlStore -> Store -> Store
modifyControlStore s' st = st { controlValues = s' }

alloc :: Map.Map k v -> Location
alloc = Map.size

type EvalValue = Either Error Value
type ExecValue = IO (Either Error ())
type InterpreterMonad a = ReaderT Environment (ExceptT Error
                                               (StateT Store IO)) a
type EvalMonad = InterpreterMonad Value
type ExecMonad = InterpreterMonad ()

formatError' :: String -> String -> Cursor -> String
formatError' e s (l, c) = concat [e, " at line ",
                                  show l, ", column ",
                                  show c, ": ", s]

formatError :: Error -> String
formatError (ArithmeticError s c) = formatError' "ArithmeticError" s c
formatError (TypeError s c) = formatError' "TypeError" s c
formatError (NameError s c) = formatError' "NameError" s c
formatError (RuntimeError s c) = formatError' "RuntimeError" s c

undeclaredVariable :: VariableName -> Cursor -> Error
undeclaredVariable x = NameError ("variable " ++ x ++ " might not have been declared")
undeclaredFunction :: FunctionName -> Cursor -> Error
undeclaredFunction fn = NameError ("function " ++ fn ++ " might not have been declared")

printError :: Error -> IO ()
printError = printStdErr . formatError

printStdErr :: String -> IO ()
printStdErr = hPutStrLn stderr

insertManyValues :: Ord a => [a] -> [b] -> Map.Map a b -> Map.Map a b
insertManyValues keys values map = foldr addOneValue map pairs where
  addOneValue (k, v) = Map.insert k v
  pairs = zip keys values

getCursor :: Expression -> Cursor
getCursor e =
  case e of
    EVar c _ -> c
    EItemInd c _ _ -> c
    EArrExp c _ -> c
    EArrDef c _ _ -> c
    ETupLit c _ -> c
    ELitInt c _ -> c
    ELitTrue c -> c
    ELitFalse c -> c
    EApp c _ _ -> c
    EString c _ -> c
    Neg c _ -> c
    Not c _ -> c
    EMul c _ _ _ -> c
    EAdd c _ _ _ -> c
    ERel c _ _ _ -> c
    EAnd c _ _ -> c
    EOr c _ _ -> c