module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import System.Exit (exitFailure, exitSuccess)

import ParGrammar
import ErrM

import CommonDeclarations
import TypeChecker
import Statements


fromMaybe :: Maybe a -> a
fromMaybe (Just x) = x
fromMaybe Nothing = error "parse tree transform error"

parse :: Maybe String -> IO CursorProgram
parse file = do
  contents <- maybe getContents readFile file
  case pProgram $ myLexer contents of
    Bad s -> do
        putStrLnStdErr s
        exitFailure
    Ok tree -> return $ fmap fromMaybe tree


main :: IO ()
main = do
  args <- getArgs
  let
    file = case args of
      (s:_) -> Just s
      [] -> Nothing
  tree <- parse file
  checkedTree <- checkTypes tree
  case checkedTree of
    Right t -> run t
    Left e -> printError e
  exitSuccess
