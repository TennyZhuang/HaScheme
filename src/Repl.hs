module Repl where

import System.IO
import System.Console.ANSI
import System.Exit (exitSuccess)
import Data.List (isPrefixOf, find)
import Control.Monad
import Control.Monad.Except
import Text.Parsec (parse)
import System.Console.Haskeline
import System.Console.Haskeline.History (historyLines)
import System.Console.Haskeline.Completion

import Lexer (reservedNames)
import AST
import Parser
import Interpreter
import Interpreter.Define
import Interpreter.Environment (builtInEnv, opMap)

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
showAST expr = case parse parseTopLevel "Scheme" expr of
  Left err -> outputStrLn $ "No match: " `mappend` show err
  Right ast -> liftIO $ showExpr stdout 0 ast

showExpr :: Handle -> Int -> Expr -> IO ()
showExpr handler n ast = do
  hPutStr handler $ replicate n ' '
  case ast of
    TopLevelExpr exprs -> sequence_ $ fmap (showExpr handler n) exprs
    FuncCallExpr x y -> do
      setSGR [SetColor Foreground Vivid Blue]
      hPutStrLn handler "FuncCallExpr"
      setSGR [Reset]
      showExpr handler (n + 2) x
      showExpr handler (n + 2) y
    LambdaFuncExpr ss x -> do
      setSGR [SetColor Foreground Vivid Yellow]
      hPutStrLn handler $ "LambdaFuncExpr " `mappend` show ss
      setSGR [Reset]
      showExpr handler (n + 2) x
    DefineVarExpr s x -> do
      setSGR [SetColor Foreground Vivid Green]
      hPutStrLn handler $ "DefineVarExpr " `mappend` show s
      setSGR [Reset]
      showExpr handler (n + 2) x
    SetVarExpr s x -> do
      setSGR [SetColor Foreground Vivid Magenta]
      hPutStrLn handler $ "SetVarExpr " `mappend` show s
      setSGR [Reset]
      showExpr handler (n + 2) x
    ConsExpr (x, y) -> do
      setSGR [SetColor Foreground Vivid Cyan]
      hPutStrLn handler "ConsExpr"
      showExpr handler (n + 2) x
      showExpr handler (n + 2) y
      setSGR [Reset]
    IfExpr x y z -> do
      setSGR [SetColor Foreground Vivid Blue]
      hPutStrLn handler "IfExpr"
      showExpr handler (n + 2) x
      showExpr handler (n + 2) y
      showExpr handler (n + 2) z
      setSGR [Reset]
    ListExpr xs -> do
      setSGR [SetColor Foreground Vivid Yellow]
      hPutStrLn handler "ListExpr ("
      sequence_ $ fmap (showExpr handler (n + 2)) xs
      setSGR [SetColor Foreground Vivid Yellow]
      hPutStr handler $ replicate n ' '
      hPutStrLn handler ")\n"
      setSGR [Reset]
    BeginExpr x -> do
      setSGR [SetColor Foreground Vivid Yellow]
      hPutStrLn handler "BeginExpr"
      setSGR [Reset]
      showExpr handler (n + 2) x
    _ -> do
      setSGR [SetColor Foreground Vivid Cyan]
      hPrint handler ast
      setSGR [Reset]

evalAndPrint :: Environment -> String -> IO ()
evalAndPrint env expr = case parse parseTopLevel "Scheme" expr of
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
  let ast = fromRight $ parse parseTopLevel "Scheme" expr
  env <- builtInEnv
  unwrapIOThrows $ eval env ast

hEvalToIO :: String -> Handle -> IO ()
hEvalToIO expr handler = case parse parseTopLevel "Scheme" expr of
  Left err -> hPrint handler  $ "No match: " `mappend` show err
  Right ast -> do
    env <- builtInEnv
    res <- runExceptT $ eval env ast
    case res of
      Left err -> hPrint handler $ show err
      Right val -> hPrint handler $ show val

wordsToComplete :: [String]
wordsToComplete = fmap fst opMap `mappend` reservedNames

completeAction :: String -> [Completion]
completeAction s = let
  filteredWords = filter (isPrefixOf s) wordsToComplete
  replaceWord str = if length filteredWords == 1 then (str, True) else (s, False)
  createCompletion str = let (t1, t2) = replaceWord str in Completion t1 str t2
  in fmap createCompletion filteredWords

completeRule :: CompletionFunc IO
completeRule = completeWord Nothing " ()\n\t\b" (return . completeAction)

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
        let l = tail $ historyLines history
        let lastInput = find (isPrefixOf ":i") l
        case lastInput of
          Just x -> showAST (drop 2 x)
          Nothing -> liftIO $ showErr "No last command"
      Just input -> if ":i" `isPrefixOf` input
        then liftIO $ evalAndPrint env (drop 2 input)
        else liftIO $ showErr "Please start with :i, :t, :q or :h"
    loop env
  in runInputT (setComplete completeRule defaultSettings) (liftIO builtInEnv >>= loop)
