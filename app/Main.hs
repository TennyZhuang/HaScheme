module Main where

import System.IO
import System.Environment
import Control.Monad
import Text.Parsec (parse)

import Parser
import Interpreter
import Repl

resultToFile :: String -> String -> IO ()
resultToFile input output = do
  inh <- openFile input ReadMode
  case output of
    "stdout" -> handleExpr inh stdout
    _ -> do
      onh <- openFile output WriteMode
      handleExpr inh onh
      hClose onh
  hClose inh

handleExpr :: Handle -> Handle -> IO ()
handleExpr inh onh = do
  expr <- hIsEOF inh
  unless expr $ do
    expr <- hGetContents inh
    hEvalToIO expr onh

astToFile :: String -> String -> IO ()
astToFile input output = do
  inh <- openFile input ReadMode
  case output of
    "stdout" -> handleAST inh stdout
    _ -> do
      onh <- openFile output WriteMode
      handleAST inh onh
      hClose onh
  hClose inh

handleAST :: Handle -> Handle -> IO ()
handleAST inh onh = do
  expr <- hIsEOF inh
  unless expr $ do
    expr <- hGetLine inh
    case parse parseTopLevel "Scheme" expr of
      Left err -> hPrint onh $ "No match: " `mappend` show err
      Right ast -> showExpr onh 0 ast
    handleAST inh onh

main :: IO ()
main = do
  args <- getArgs
  case length args of
    1 -> case head args of
      "-repl" -> runRepl
      "-i" -> putStrLn "-i <inputFile> [-o <outputFile>]"
      "-t" -> putStrLn "-t <inputFile> [-o <outputFile>]"
      _ -> putStrLn "Arguments Error."
    2 -> case head args of
      "-i" -> resultToFile (last args) "stdout"
      "-t" -> astToFile (last args) "stdout"
      _ -> putStrLn "Arguments Error."
    4 -> case head args of
      "-i" -> resultToFile (args !! 1) (last args)
      "-t" -> astToFile (args !! 1) (last args)
      _ -> putStrLn "Arguments Error."
    _ -> putStrLn "Arguments Error."
