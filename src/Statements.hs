module Statements (execStmt, run) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Char

import AbsGrammar
import CommonDeclarations
import Expressions

run :: CursorProgram -> IO ()
run (Program _ l) = do
  evaluated <- execStmt (Sequence l)
  -- HERE
  case evaluated of
    Right _ -> return ()
    Left e -> liftIO $ printError e

execStmt :: Statement -> ExecValue
execStmt i = evalStateT (runExceptT (runReaderT (exec i) r)) s where
  r = emptyEnvironment
  s = emptyStore

processExpValue :: Expression -> (Value -> ExecMonad) -> ExecMonad
processExpValue e f = do
  s <- get
  r <- ask
  let v = evalExp e r s
  case v of
    Right n -> f n
    Left err -> throwError err

execBlock :: (Block Cursor) -> ExecMonad
execBlock (Block _ is) = exec (Sequence is)

exec :: Statement -> ExecMonad
exec (Empty _) = return ()
exec (Sequence []) = return ()
exec (Sequence [i]) = exec i
exec (Sequence (i:is)) = do
  exec i
  exec (Sequence is)
exec (Print _ e) = processExpValue e printValue where
  printValue (VInt n) = liftIO $ print n
  printValue (VString s) = liftIO $ putStrLn s
  printValue (VBool b) = liftIO $ putStrLn $ map toLower (show b)
exec (SExp _ e) = processExpValue e (const $ return ())
exec (Cond cur e bl) = do
  s <- get
  r <- ask
  b <- evalExp e r s
  case b of
    (VBool v) -> execBlock bl
    other -> throwError $ TypeError ("incompatible type in an if statement " ++
                                     show other) cur

-- exec (CondElse _ e caseTrue caseFalse)
-- exec (Cond a (Expr a) (Block a))
-- exec (CondElse a (Expr a) (Block a) (Block a))
