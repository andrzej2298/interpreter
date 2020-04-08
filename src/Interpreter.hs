module Main where

import System.Environment ( getArgs )
import System.IO ( readFile )
import System.Exit ( exitFailure, exitSuccess )

-- import AbsGrammar
import LexGrammar
import ParGrammar
import ErrM

import CommonDeclarations
import TypeChecker
import Run


fromMaybe :: Maybe a -> a
fromMaybe (Just x) = x
fromMaybe Nothing = error "parse tree transform error"

parse :: String -> IO CursorProgram
parse file = do
  file <- readFile file
  case pProgram $ myLexer file of
    Bad s -> do
        putStrLn "failure"
        exitFailure
    Ok tree -> do
        putStrLn "success"
        return $ fmap fromMaybe tree

main :: IO ()
main = do
  (file:_) <- getArgs
  tree <- parse file
  checkTypes tree
  run tree
  exitSuccess

