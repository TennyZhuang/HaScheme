module Interpreter where

import Control.Monad.Except
import Data.Maybe (fromJust)

import AST

data SchemeValue =
  SchemeNumber Double |
  SchemeBool Bool |
  SchemeList [SchemeValue] deriving (Show)

data SyntaxError =
  ArgsNumber Int [SchemeValue] |
  TypeMismatch String SchemeValue |
  Unknown

instance Show SyntaxError where
  show Unknown = "Unknown Error"

type ThrowsError = Either SyntaxError

unwrapNumber :: SchemeValue -> ThrowsError Double
unwrapNumber (SchemeNumber num) = return num
unwrapNumber arg = throwError $ TypeMismatch "number" arg

unwrapBool :: SchemeValue -> ThrowsError Bool
unwrapBool (SchemeBool b) = return b
unwrapBool arg = throwError $ TypeMismatch "bool" arg

unwrapList :: SchemeValue -> ThrowsError [SchemeValue]
unwrapList (SchemeList l) = return l
unwrapList arg = throwError $ TypeMismatch "list" arg

schemeNot :: [SchemeValue] -> ThrowsError SchemeValue
schemeNot [SchemeBool b] = return . SchemeBool $ not b
schemeNot [arg] = throwError $ TypeMismatch "bool" arg
schemeNot args = throwError $ ArgsNumber 1 args

numberNumberOp :: (Double -> Double -> Double) -> [SchemeValue] -> ThrowsError SchemeValue
numberNumberOp f l = fmap (SchemeNumber . foldl1 f) (sequence $ fmap unwrapNumber l)

boolBoolOp :: (Bool -> Bool -> Bool) -> [SchemeValue] -> ThrowsError SchemeValue
boolBoolOp f l = fmap (SchemeBool . foldl1 f) (sequence $ fmap unwrapBool l)

opMap :: [(String, [SchemeValue] -> ThrowsError SchemeValue)]
opMap = [
  ("+", numberNumberOp (+)),
  ("-", numberNumberOp (-)),
  ("*", numberNumberOp (*)),
  ("/", numberNumberOp (/)),
  ("&&", boolBoolOp (&&)),
  ("||", boolBoolOp (||)),
  ("not", schemeNot)]

eval :: Expr -> ThrowsError SchemeValue
eval (NumberExpr x) = return $ SchemeNumber x
eval (BoolExpr b) = return $ SchemeBool b
eval (ListExpr l) = fmap SchemeList (sequence $ fmap eval l)
eval (ReservedOpCallExpr op args) = eval args >>= unwrapList >>= fromJust (lookup op opMap)
