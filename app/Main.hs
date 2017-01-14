module Main where

import System.IO
import System.Environment
import Text.Parsec (parse)

import Parser
import Interpreter
import Repl

getFileContent :: String -> IO String
getFileContent input = do
  inh <- openFile input ReadMode
  expr <- hGetLine inh
  hClose inh
  return expr

resultToFile :: String -> String -> IO ()
resultToFile input output = do
  expr <- getFileContent input
  result <- evalAnyWay expr
  case output of
    "stdout" -> print result
    _ -> do
      onh <- openFile output WriteMode
      hPrint onh result
      hClose onh

astToFile :: String -> String -> IO ()
astToFile input output = do
  expr <- getFileContent input
  case parse parseExpr "Scheme" expr of
    Left err -> putStrLn $ "No match: " `mappend` show err
    Right ast -> case output of
      "stdout" -> showExpr 0 ast
      _ -> do
        onh <- openFile output WriteMode
        hPrint onh ast
        hClose onh

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
