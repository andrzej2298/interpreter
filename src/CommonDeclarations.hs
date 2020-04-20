module CommonDeclarations where

import System.IO (hPutStrLn, stderr)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map

import AbsGrammar

type Cursor = (Int, Int)
type Expression = Expr Cursor
type Statement = Stmt Cursor
type CursorProgram = Program Cursor

type VariableName = String
type FunctionName = String
type Location = Int

data Value = VInt Integer | VBool Bool | VString String
type Function = [Value] -> EvalMonad

defaultValue :: Type Cursor -> Value
defaultValue (Int _) = VInt 0
defaultValue (Str _) = VString ""
defaultValue (Bool _) = VBool False

instance Show Value where
  show (VInt _) = "Integer"
  show (VBool _) = "Bool"
  show (VString _) = "String"

data Error =
  ArithmeticError String Cursor |
  TypeError String Cursor |
  NameError String Cursor

data Environment = Environment { functions :: FunctionEnvironment, variables :: VariableEnvironment }
type VariableEnvironment = Map.Map VariableName Location
type FunctionEnvironment = Map.Map FunctionName Function
type Store = Map.Map Location Value

emptyEnvironment :: Environment
emptyEnvironment = Environment { functions = Map.empty, variables = Map.empty }
declareVariable :: VariableName -> Location -> Environment -> Environment
declareVariable x l env = env { variables = Map.insert x l (variables env) }
declareFunction :: FunctionName -> Function -> Environment -> Environment
declareFunction fn f env = env { functions = Map.insert fn f (functions env) }
emptyStore :: Store
emptyStore = Map.empty :: Store

alloc :: Store -> Location
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

undeclaredVariable :: String -> Cursor -> Error
undeclaredVariable x = NameError ("variable " ++ x ++ " might not have been declared")
undeclaredFunction :: String -> Cursor -> Error
undeclaredFunction fn = NameError ("function " ++ fn ++ " might not have been declared")

printError :: Error -> IO ()
printError = printStdErr . formatError

printStdErr :: String -> IO ()
printStdErr = hPutStrLn stderr
