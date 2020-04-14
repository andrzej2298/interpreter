module CommonDeclarations where

import System.Exit (exitFailure, exitSuccess)
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
type Value = Integer
data Error = ArithmeticError String Cursor

type Environment = Map.Map Variable Location
type Store = Map.Map Location Value

emptyEnvironment = Map.empty :: Environment
emptyStore = Map.empty :: Store

type EvalValue = Either Error Value
type ExecValue = IO (Either Error ())
type EvalMonad = ReaderT Environment (ExceptT Error
                                      (StateT Store Identity)) Value
type ExecMonad = ReaderT Environment (ExceptT Error
                                      (StateT Store IO)) ()

formatError :: Error -> String
formatError (ArithmeticError s (l, c)) = concat ["error at line ",
                                                 show l, ", column ",
                                                 show c, ": ", s]

printError :: Error -> IO ()
printError = printStdErr . formatError

printStdErr :: String -> IO ()
printStdErr = hPutStrLn stderr
