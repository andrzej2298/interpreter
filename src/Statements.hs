module Statements (execStmt, run) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import AbsGrammar
import CommonDeclarations
import Expressions

run :: CursorProgram -> IO ()
run (Program _ (i:_)) = do
  -- let x = evalExp e emptyEnvironment emptyStore
  -- print x
  evaluated <- execStmt i
  case evaluated of
    Right _ -> return ()
    Left e -> liftIO $ printError e

execStmt :: Statement -> ExecValue
execStmt i = evalStateT (runExceptT (runReaderT (exec i) r)) s where
  r = emptyEnvironment
  s = emptyStore

test :: IO String
test = return "abc"

-- execIO :: EvalValue -> ExecMonad
-- execIO m = do

processExpValue :: Expression -> (Value -> ExecMonad) -> ExecMonad
processExpValue e f = do
  s <- get
  r <- ask
  let v = evalExp e r s
  case v of
    Right n -> f n
    Left e -> throwError e

exec :: Statement -> ExecMonad
exec (Print _ e) = processExpValue e (\n -> do liftIO $ print n)
exec (SExp _ e) = processExpValue e (\_ -> return ())
