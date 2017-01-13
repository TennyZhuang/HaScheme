module Repl where

import System.IO
import System.Console.ANSI
import Control.Monad
import Control.Monad.Except
import Text.Parsec (parse)
import System.Console.Haskeline

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

evalAndPrint :: Environment -> String -> IO ()
evalAndPrint env expr = case parse parseExpr "Scheme" expr of
  Left err -> showErr $ "No match: " `mappend` show err
  Right ast -> do
    res <- runExceptT $ eval env ast
    case res of
      Left err -> showErr $ show err
      Right val -> showRes $ show val

runRepl :: IO ()
runRepl = let
  loop :: Environment -> InputT IO ()
  loop env = do
    minput <- getInputLine "Scheme >>> "
    case minput of
      Nothing -> return ()
      Just ":q" -> return ()
      Just input -> liftIO $ evalAndPrint env input
    loop env
  in runInputT defaultSettings (liftIO builtInEnv >>= loop)
