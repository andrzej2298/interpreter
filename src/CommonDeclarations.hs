module CommonDeclarations where

import System.IO (hPutStrLn, hPrint, stderr)
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
data ControlParameter = ReturnParameter | BreakParameter | ContinueParameter
  deriving (Eq, Ord, Show)
type Location = Int

data Value =
  VInt Integer |
  VBool Bool |
  VString String |
  VoidValue |
  VArray (Vector.Vector Value) |
  VTuple (Vector.Vector Value)
  deriving (Eq)

type Function = [Expression] -> EvalMonad
data ControlValue = ReturnValue (Maybe Value) | Flag Bool
  deriving (Show)

dummyCursor :: Cursor
dummyCursor = (0, 0)

defaultValue :: Type Cursor -> Value
defaultValue (Int _) = VInt 0
defaultValue (Bool _) = VBool False
defaultValue (Str _) = VString ""
defaultValue (Array _ _) = VArray Vector.empty
defaultValue (Void _) = VoidValue
defaultValue (Tuple _ types) = VTuple (Vector.fromList (map defaultValue types))

instance Show Value where
  show v = showsValue v "" where
    showsValue (VInt i) = shows i
    showsValue (VBool b) = showString $ map toLower (show b)
    showsValue (VString s) = showString "\"" . showString s . showString "\""
    showsValue (VArray vec) = showString "[" . showString (intercalate ", " $ map show (Vector.toList vec)) . showString "]"
    showsValue VoidValue = showString "void"
    showsValue (VTuple vec) = showString "<" . showString (intercalate ", " $ map show (Vector.toList vec)) . showString ">"

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
declareControlValue :: ControlParameter -> Location -> Environment -> Environment
declareControlValue p l env = env { control = Map.insert p l (control env) }
declareControlValues :: [ControlParameter] -> [Location] -> Environment -> Environment
declareControlValues ps ls env = env { control = insertManyValues ps ls (control env) }

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
printError = putStrLnStdErr . formatError

putStrLnStdErr :: String -> IO ()
putStrLnStdErr = hPutStrLn stderr

printStdErr :: Show a => a -> IO ()
printStdErr = hPrint stderr

insertManyValues :: Ord a => [a] -> [b] -> Map.Map a b -> Map.Map a b
insertManyValues keys vals kvMap = foldr addOneValue kvMap pairs where
  addOneValue (k, v) = Map.insert k v
  pairs = zip keys vals

generateLocationsForValues :: Map.Map Location v -> [v] -> ([Location], Map.Map Location v)
generateLocationsForValues s =
  foldr generateSingleLocation ([], s) where
    generateSingleLocation val (locs, currentS) =
      let newLoc = alloc currentS in
        (newLoc:locs, Map.insert newLoc val currentS)

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

emptyBlock :: Block Cursor
emptyBlock = Block (0, 0) []
