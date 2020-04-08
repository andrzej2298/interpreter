module CommonDeclarations where

import System.Exit ( exitFailure, exitSuccess )
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map

import AbsGrammar

type Cursor = (Int, Int)
type Expression = Expr Cursor
type CursorProgram = Program Cursor

type Variable = String
type Location = Integer
type Value = Integer
type Error = (String, Cursor)

type Environment = Map.Map Variable Location
type Store = Map.Map Location Value

emptyEnvironment = Map.empty :: Environment
emptyStore = Map.empty :: Store

type EvalValue = Either Error Value
type EvalMonad = ReaderT Environment (ExceptT Error
                                     (StateT Store Identity)) Value
-- type ExecMonad = ReaderT Env (StateT Store IO())
