module Interpreter where

import Data.IORef
import Control.Monad.Except

import AST
import Interpreter.Define
import Interpreter.Environment
import Interpreter.Operand

apply :: Environment -> SchemeValue -> [SchemeValue] -> IOThrowsError SchemeValue
apply _ (SchemeBuiltInFunc f) args = liftThrows $ f args
apply envRef (SchemeFunc argnames body closure) args =
  if length args /= length argnames
    then throwError $ ArgsNumber (length argnames) args
    else do
      newEnv <- liftIO . bindVars closure $ zip argnames args
      eval newEnv body

eval :: Environment -> Expr -> IOThrowsError SchemeValue
eval _ (NumberExpr x) = return $ SchemeNumber x
eval _ (BoolExpr b) = return $ SchemeBool b
eval _ (CharExpr c) = return $ SchemeChar c
eval env (ListExpr l) = fmap SchemeList (sequence $ fmap (eval env) l)
eval env (ConsExpr (l, r)) = do
  left <- eval env l
  right <- eval env r
  return $ SchemeCons (left, right)
eval env (SymbolExpr varname) = getVar env varname
eval env (IfExpr condE leftE rightE) = do
  cond <- eval env condE
  condR <- liftThrows $ unwrapBool cond
  if condR
    then eval env leftE
    else eval env rightE
eval _ NilExpr = return SchemeNil
eval env (LambdaFuncExpr args body) = return $ SchemeFunc args body env
eval env (FuncCallExpr caller args) = do
  func <- eval env caller
  argsV <- eval env args
  argsL <- liftThrows $ unwrapList argsV
  apply env func argsL
eval env (DefineVarExpr varname expr) = eval env expr >>= defineVar env varname
eval env (SetVarExpr varname expr) = eval env expr >>= setVar env varname
eval env (BeginExpr exprsE) = last <$> (eval env exprsE >>= (liftThrows . unwrapList))
