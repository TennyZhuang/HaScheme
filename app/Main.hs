module Main where

import System.Environment
import Text.Parsec (parse)

import Parser
import Interpreter
import Repl

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> do
      let expr = head args
      case parse parseExpr "Scheme" expr of
        Left err -> showErr $ "No match: " `mappend` show err
        Right ast -> case eval ast of
          Left err -> showErr $ "Evaluate Error: " `mappend` show err
          Right val -> showRes $ show val
    _ -> showErr "Only 0 or 1 argument!"
