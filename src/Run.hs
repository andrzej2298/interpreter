module Run (run) where

import System.Exit ( exitFailure, exitSuccess )
import Control.Monad.Reader
import Control.Monad.State

import AbsGrammar
import CommonDeclarations
import Statements
import Expressions


run :: (Program Cursor) -> IO ()
run (Program _ [SExp _ e]) = do
    let x = evalExp e emptyEnvironment emptyStore
    print x
    -- execStmt (Block instructions)
    putStrLn "program ran"
