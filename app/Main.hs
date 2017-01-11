module Main where

import System.Environment
import System.Console.ANSI
import Text.Parsec (parse)
import Parser

main :: IO ()
main = do
  args <- getArgs
  let expr = head args
  case parse parseExpr "lisp" expr of
    Left err -> do
                setSGR [SetColor Foreground Vivid Red]
                putStr $ "No match:" `mappend` show err
                setSGR [Reset]
    Right val -> do
                 setSGR [SetColor Foreground Vivid Blue]
                 putStr $ show val
                 setSGR [Reset]
