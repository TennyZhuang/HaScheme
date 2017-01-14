module Interpreter.Define where

import Control.Monad.Except
import Data.IORef
import Data.Array

import AST

type Environment = IORef [(String, IORef SchemeValue)]

data SchemeValue =
  SchemeNumber Double |
  SchemeBool Bool |
  SchemeChar Char |
  SchemeArray (Array Int SchemeValue) |
  SchemeList [SchemeValue] |
  SchemeCons (SchemeValue, SchemeValue) |
  SchemeBuiltInFunc ([SchemeValue] -> ThrowsError SchemeValue) |
  SchemeFunc [String] Expr Environment |
  SchemeNil

instance Show SchemeValue where
  show (SchemeNumber num) = show num
  show (SchemeBool b) = if b then "#t" else "#f"
  show (SchemeChar c) = show c
  show (SchemeArray a) = concat ["[", unwords . elems $ fmap show a ,"]"]
  show (SchemeList l) = concat ["(", unwords (fmap show l) ,")"]
  show (SchemeCons (l, r)) = concat ["(", show l, " . ", show r, ")"]
  show (SchemeBuiltInFunc _) = "build-in"
  show (SchemeFunc args _ _) = concat ["lambda (", unwords args, ")"]
  show SchemeNil = "()"

typeOf :: SchemeValue -> String
typeOf (SchemeNumber _) = "number"
typeOf (SchemeBool _) = "bool"
typeOf (SchemeChar _) = "char"
typeOf (SchemeArray _) = "array"
typeOf (SchemeList _) = "list"
typeOf (SchemeCons _) = "cons"
typeOf (SchemeBuiltInFunc _) = "function"
typeOf SchemeFunc {} = "function"
typeOf SchemeNil = "nil"

data SyntaxError =
  ArgsNumber Int [SchemeValue] |
  TypeMismatch String SchemeValue |
  UnboundVariable String |
  Unknown

type ThrowsError = Either SyntaxError

type IOThrowsError = ExceptT SyntaxError IO

instance Show SyntaxError where
  show (ArgsNumber i args) = concat [
    "Expect ", show i, " args, get ", show $ length args, "\n",
    "Actual args: ", show args]
  show (TypeMismatch t arg) = concat [
    "Expect ", t, ", get ", typeOf arg, "\n",
    "Actual arg: ", show arg]
  show (UnboundVariable varname) = "Unbound Variable: " `mappend` varname
  show Unknown = "Unknown Error"

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

unwrapIOThrows :: IOThrowsError a -> IO a
unwrapIOThrows ev = do
  v <- runExceptT ev
  case v of
    (Left err) -> error "error"
    (Right val) -> return val
