module Interpreter where

import Control.Monad.Except
import Data.Maybe (fromJust)

import AST

data SchemeValue =
  SchemeNumber Double |
  SchemeList [SchemeValue] deriving (Show)

data SyntaxError =
  ArgsNumber Int [SchemeValue] |
  TypeMismatch String SchemeValue |
  Unknown

instance Show SyntaxError where
  show Unknown = "Unknown Error"

type ThrowsError = Either SyntaxError

convert :: Expr -> ThrowsError SchemeValue
convert (NumberExpr x) = return $ SchemeNumber x
convert (ListExpr l) = fmap SchemeList (sequence $ fmap convert l)

unwrapNumber :: SchemeValue -> ThrowsError Double
unwrapNumber (SchemeNumber num) = return num
unwrapNumber arg = throwError $ TypeMismatch "number" arg

unwrapList :: SchemeValue -> ThrowsError [SchemeValue]
unwrapList (SchemeList l) = return l
unwrapList arg = throwError $ TypeMismatch "list" arg

numberNumberOp :: (Double -> Double -> Double) -> [SchemeValue] -> ThrowsError SchemeValue
numberNumberOp f l = fmap (SchemeNumber . foldl1 f) (sequence $ fmap unwrapNumber l)

opMap :: [(String, [SchemeValue] -> ThrowsError SchemeValue)]
opMap = [
  ("+", numberNumberOp (+)),
  ("-", numberNumberOp (-)),
  ("*", numberNumberOp (*)),
  ("/", numberNumberOp (/))]

eval :: Expr -> ThrowsError SchemeValue
eval (ReservedOpCallExpr op args) = convert args >>= unwrapList >>= fromJust (lookup op opMap)
