module Repl where

import System.IO
import System.Console.ANSI
import Control.Monad
import Text.Parsec (parse)

import AST
import Parser
import Interpreter

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

showErr :: String -> IO ()
showErr err = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn err
  setSGR [Reset]

showRes :: String -> IO ()
showRes res = do
  setSGR [SetColor Foreground Vivid Blue]
  putStrLn res
  setSGR [Reset]

evalAndPrint :: String -> IO ()
evalAndPrint expr = case parse parseExpr "Scheme" expr of
  Left err -> showErr $ "No match: " `mappend` show err
  Right ast -> case eval ast of
    Left err -> showErr $ "Evaluate Error: " `mappend` show err
    Right val -> showRes $ show val

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  unless (pred result) $ action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Scheme>>> ") evalAndPrint
