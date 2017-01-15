module Interpreter.Environment where

import Interpreter.Define
import Interpreter.Operand

import Data.IORef
import qualified Data.Map as Map
import Control.Monad.Except

opMap :: [(String, [SchemeValue] -> ThrowsError SchemeValue)]
opMap = [
  ("+", numberNumberOp (+)),
  ("-", numberNumberOp (-)),
  ("*", numberNumberOp (*)),
  ("/", numberNumberOp (/)),
  ("and", boolBoolOp (&&)),
  ("or", boolBoolOp (||)),
  ("not", schemeNot),
  ("<", numberBoolOp (<)),
  (">", numberBoolOp (>)),
  ("=", numberBoolOp (==)),
  ("<=", numberBoolOp (<=)),
  (">=", numberBoolOp (>=)),
  ("car", schemeCar),
  ("cdr", schemeCdr),
  ("cons", schemeCons),
  ("vector-ref", schemeVectorRef)]

nullEnv :: IO Environment
nullEnv = newIORef Map.empty

builtInEnv :: IO Environment
builtInEnv = do
  env <- nullEnv
  bindVars env ((fmap . fmap) SchemeBuiltInFunc opMap)

getVar :: Environment -> String -> IOThrowsError SchemeValue
getVar envRef varname = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVariable varname)
        (liftIO . readIORef)
        (Map.lookup varname env)

setVar :: Environment -> String -> SchemeValue -> IOThrowsError SchemeValue
setVar envRef varname val = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVariable varname)
        (liftIO . (`writeIORef` val))
        (Map.lookup varname env)
  return val

defineVar :: Environment -> String -> SchemeValue -> IOThrowsError SchemeValue
defineVar envRef varname val = do
  env <- liftIO $ readIORef envRef
  liftIO $ do
    valRef <- newIORef val
    writeIORef envRef (Map.insert varname valRef env)
    return val

bindVars :: Environment -> [(String, SchemeValue)] -> IO Environment
bindVars envRef bindings = let
  newEnvRefIO = do
    env <- readIORef envRef
    newIORef env
  in do
    newEnvRef <- newEnvRefIO
    sequence_ $ fmap (\(varname, val) -> runExceptT $ defineVar newEnvRef varname val) bindings
    return newEnvRef
