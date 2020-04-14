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
  contents <- case file of
    Just f -> readFile f
    Nothing -> getContents
  case pProgram $ myLexer contents of
    Bad s -> do
        printStdErr s
        exitFailure
    Ok tree -> do
        return $ fmap fromMaybe tree


main :: IO ()
main = do
  args <- getArgs
  let file = case args of
        (s:_) -> Just s
        [] -> Nothing
  tree <- parse file
  checkTypes tree
  run tree
  exitSuccess

