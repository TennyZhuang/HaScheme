module Interpreter where

import Control.Monad.Except
import Data.Maybe (fromJust)

import AST

data SchemeValue =
  SchemeNumber Double |
  SchemeBool Bool |
  SchemeList [SchemeValue]

data SyntaxError =
  ArgsNumber Int [SchemeValue] |
  TypeMismatch String SchemeValue |
  Unknown

instance Show SchemeValue where
  show (SchemeNumber num) = show num
  show (SchemeBool b) = if b then "#t" else "#f"
  show (SchemeList l) = concat ["(", unwords (fmap show l) ,")"]

typeOf :: SchemeValue -> String
typeOf (SchemeNumber _) = "number"
typeOf (SchemeBool _) = "bool"
typeOf (SchemeList _) = "list"

instance Show SyntaxError where
  show (ArgsNumber i args) = concat [
    "Expect ", show i, " args, get ", show $ length args, "\n",
    "Actual args: ", show args]
  show (TypeMismatch t arg) = concat [
    "Expect ", t, ", get ", typeOf arg, "\n",
    "Actual arg: ", show arg]
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

schemeCar :: [SchemeValue] -> ThrowsError SchemeValue
schemeCar [SchemeList l] = return $ head l
schemeCar [arg] = throwError $ TypeMismatch "list" arg
schemeCar args = throwError $ ArgsNumber 1 args

schemeCdr :: [SchemeValue] -> ThrowsError SchemeValue
schemeCdr [SchemeList l] = return . SchemeList $ tail l
schemeCdr [arg] = throwError $ TypeMismatch "list" arg
schemeCdr args = throwError $ ArgsNumber 1 args

numberNumberOp :: (Double -> Double -> Double) -> [SchemeValue] -> ThrowsError SchemeValue
numberNumberOp f l = fmap (SchemeNumber . foldl1 f) (sequence $ fmap unwrapNumber l)

boolBoolOp :: (Bool -> Bool -> Bool) -> [SchemeValue] -> ThrowsError SchemeValue
boolBoolOp f l = fmap (SchemeBool . foldl1 f) (sequence $ fmap unwrapBool l)

numberBoolOp :: (Double -> Double -> Bool) -> [SchemeValue] -> ThrowsError SchemeValue
numberBoolOp f [SchemeNumber arg1, SchemeNumber arg2] =
  return $ SchemeBool (f arg1 arg2)
numberBoolOp _ [SchemeNumber _, arg2] = throwError $ TypeMismatch "number" arg2
numberBoolOp _ [arg1, _] = throwError $ TypeMismatch "number" arg1
numberBoolOp _ args = throwError $ ArgsNumber 2 args

opMap :: [(String, [SchemeValue] -> ThrowsError SchemeValue)]
opMap = [
  ("+", numberNumberOp (+)),
  ("-", numberNumberOp (-)),
  ("*", numberNumberOp (*)),
  ("/", numberNumberOp (/)),
  ("&&", boolBoolOp (&&)),
  ("||", boolBoolOp (||)),
  ("not", schemeNot),
  ("<", numberBoolOp (<)),
  (">", numberBoolOp (>)),
  ("=", numberBoolOp (==)),
  ("car", schemeCar),
  ("cdr", schemeCdr)]

eval :: Expr -> ThrowsError SchemeValue
eval (NumberExpr x) = return $ SchemeNumber x
eval (BoolExpr b) = return $ SchemeBool b
eval (ListExpr l) = fmap SchemeList (sequence $ fmap eval l)
eval (ReservedOpCallExpr op args) = eval args >>= unwrapList >>= fromJust (lookup op opMap)
