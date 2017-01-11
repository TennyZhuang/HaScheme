module Main where

import System.Environment
import Text.Parsec (parse)
import Parser

main :: IO ()
main = do
  args <- getArgs
  let expr = head args
  print $ case parse parseExpr "lisp" expr of
    Left err -> "No match:" `mappend` show err
    Right val -> show val
