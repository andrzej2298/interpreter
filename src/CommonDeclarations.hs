module CommonDeclarations where

import System.IO (hPutStrLn, stderr)
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map

import AbsGrammar

type Cursor = (Int, Int)
type Expression = Expr Cursor
type Statement = Stmt Cursor
type CursorProgram = Program Cursor

type Variable = String
type Location = Integer

data Value = VInt Integer | VBool Bool | VString String
instance Show Value where
  show (VInt _) = "Integer"
  show (VBool _) = "Bool"
  show (VString _) = "String"

data Error =
  ArithmeticError String Cursor |
  TypeError String Cursor

type Environment = Map.Map Variable Location
type Store = Map.Map Location Value

emptyEnvironment :: Environment
emptyEnvironment = Map.empty
emptyStore :: Store
emptyStore = Map.empty :: Store

type EvalValue = Either Error Value
type ExecValue = IO (Either Error ())
type EvalMonad = ReaderT Environment (ExceptT Error
                                      (StateT Store Identity)) Value
type ExecMonad = ReaderT Environment (ExceptT Error
                                      (StateT Store IO)) ()

formatError' :: String -> String -> Cursor -> String
formatError' e s (l, c) = concat [e, " at line ",
                                  show l, ", column ",
                                  show c, ": ", s]

formatError :: Error -> String
formatError (ArithmeticError s c) = formatError' "ArithmeticError" s c
formatError (TypeError s c) = formatError' "TypeError" s c

printError :: Error -> IO ()
printError = printStdErr . formatError

printStdErr :: String -> IO ()
printStdErr = hPutStrLn stderr
