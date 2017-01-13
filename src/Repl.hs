module Repl where

import System.IO
import System.Console.ANSI
import System.Exit (exitSuccess)
import Control.Monad
import Control.Monad.Except
import Text.Parsec (parse)
import System.Console.Haskeline
import System.Console.Haskeline.History (historyLines)

import AST
import Parser
import Interpreter
import Interpreter.Define
import Interpreter.Environment (builtInEnv)

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

showAST :: String -> String
showAST expr = case parse parseExpr "Scheme" expr of
  Left err -> "No match: " `mappend` show err
  Right ast -> show ast

evalAndPrint :: Environment -> String -> IO ()
evalAndPrint env expr = case parse parseExpr "Scheme" expr of
  Left err -> showErr $ "No match: " `mappend` show err
  Right ast -> do
    res <- runExceptT $ eval env ast
    case res of
      Left err -> showErr $ show err
      Right val -> showRes $ show val

fromRight :: Either a b -> b
fromRight (Left _) = error ""
fromRight (Right v) = v

evalAnyWay :: String -> IO SchemeValue
evalAnyWay expr = do
  let ast = fromRight $ parse parseExpr "Scheme" expr
  env <- builtInEnv
  unwrapIOThrows $ eval env ast

runRepl :: IO ()
runRepl = let
  loop :: Environment -> InputT IO ()
  loop env = do
    minput <- getInputLine "Scheme >>> "
    case minput of
      Nothing -> return ()
      Just ":q" -> liftIO exitSuccess
      Just ":t" -> do
        history <- getHistory
        outputStrLn . showAST . head . tail $ historyLines history -- Error handler
      Just input -> liftIO $ evalAndPrint env input
    loop env
  in runInputT defaultSettings (liftIO builtInEnv >>= loop)
