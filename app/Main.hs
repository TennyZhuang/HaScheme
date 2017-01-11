module Main where

import System.Environment
import System.Console.ANSI
import Text.Parsec (parse)
import Parser
import Interpreter

showErr :: String -> IO ()
showErr err = do
  setSGR [SetColor Foreground Vivid Red]
  putStr err
  setSGR [Reset]

showRes :: String -> IO ()
showRes res = do
  setSGR [SetColor Foreground Vivid Blue]
  putStr res
  setSGR [Reset]

main :: IO ()
main = do
  args <- getArgs
  let expr = head args
  case parse parseExpr "lisp" expr of
    Left err -> showErr $ "No match: " `mappend` show err
    Right ast -> case eval ast of
      Left err -> showErr $ "Evaluate Error: " `mappend` show err
      Right val -> showRes $ show val
