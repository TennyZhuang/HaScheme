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

showAST :: String -> InputT IO ()
showAST expr = case parse parseExpr "Scheme" expr of
  Left err -> outputStrLn $ "No match: " `mappend` show err
  Right ast -> liftIO $ showExpr 0 ast

showExpr :: Int -> Expr -> IO ()
showExpr n ast = do
  putStr $ replicate n ' '
  case ast of
    FuncCallExpr x y -> do
      setSGR [SetColor Foreground Vivid Blue]
      putStrLn "FuncCallExpr"
      setSGR [Reset]
      showExpr (n + 2) x
      showExpr (n + 2) y
    LambdaFuncExpr ss x -> do
      setSGR [SetColor Foreground Vivid Yellow]
      putStrLn $ "LambdaFuncExpr " `mappend` show ss
      setSGR [Reset]
      showExpr (n + 2) x
    DefineVarExpr s x -> do
      setSGR [SetColor Foreground Vivid Green]
      putStrLn $ "DefineVarExpr " `mappend` show s
      setSGR [Reset]
      showExpr (n + 2) x
    SetVarExpr s x -> do
      setSGR [SetColor Foreground Vivid Magenta]
      putStrLn $ "SetVarExpr " `mappend` show s
      setSGR [Reset]
      showExpr (n + 2) x
    ConsExpr (x, y) -> do
      setSGR [SetColor Foreground Vivid Cyan]
      putStrLn "ConsExpr"
      showExpr (n + 2) x
      showExpr (n + 2) y
      setSGR [Reset]
    IfExpr x y z -> do
      setSGR [SetColor Foreground Vivid Blue]
      putStrLn "IfExpr"
      showExpr (n + 2) x
      showExpr (n + 2) y
      showExpr (n + 2) z
      setSGR [Reset]
    ListExpr xs -> do
      setSGR [SetColor Foreground Vivid Yellow]
      putStrLn "ListExpr"
      setSGR [Reset]
      showExpr (n + 2) $ AST.fromList xs
    BeginExpr x -> do
      setSGR [SetColor Foreground Vivid Yellow]
      putStrLn "BeginExpr"
      setSGR [Reset]
      showExpr (n + 2) x
    _ -> do
      setSGR [SetColor Foreground Vivid Cyan]
      print ast
      setSGR [Reset]

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
        showAST . head . tail $ historyLines history -- Error handler
      Just input -> liftIO $ evalAndPrint env input
    loop env
  in runInputT defaultSettings (liftIO builtInEnv >>= loop)
