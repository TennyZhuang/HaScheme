module Interpreter where

import Control.Monad.Except
import Data.Maybe (fromJust, isJust)
import Data.IORef

import AST

data SchemeValue =
  SchemeNumber Double |
  SchemeBool Bool |
  SchemeList [SchemeValue] |
  SchemeCons (SchemeValue, SchemeValue) |
  SchemeNil

data SyntaxError =
  ArgsNumber Int [SchemeValue] |
  TypeMismatch String SchemeValue |
  UnboundVariable String |
  Unknown

instance Show SchemeValue where
  show (SchemeNumber num) = show num
  show (SchemeBool b) = if b then "#t" else "#f"
  show (SchemeList l) = concat ["(", unwords (fmap show l) ,")"]
  show (SchemeCons (l, r)) = concat ["(", show l, " . ", show r, ")"]
  show SchemeNil = "()"

typeOf :: SchemeValue -> String
typeOf (SchemeNumber _) = "number"
typeOf (SchemeBool _) = "bool"
typeOf (SchemeList _) = "list"
typeOf (SchemeCons _) = "cons"
typeOf SchemeNil = "nil"

instance Show SyntaxError where
  show (ArgsNumber i args) = concat [
    "Expect ", show i, " args, get ", show $ length args, "\n",
    "Actual args: ", show args]
  show (TypeMismatch t arg) = concat [
    "Expect ", t, ", get ", typeOf arg, "\n",
    "Actual arg: ", show arg]
  show (UnboundVariable varname) = "Unbound Variable: " `mappend` varname
  show Unknown = "Unknown Error"

type ThrowsError = Either SyntaxError

type IOThrowsError = ExceptT SyntaxError IO

type Environment = IORef [(String, IORef SchemeValue)]

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

nullEnv :: IO Environment
nullEnv = newIORef []

getVar :: Environment -> String -> IOThrowsError SchemeValue
getVar envRef varname = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVariable varname)
        (liftIO . readIORef)
        (lookup varname env)

setVar :: Environment -> String -> SchemeValue -> IOThrowsError SchemeValue
setVar envRef varname val = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVariable varname)
        (liftIO . (`writeIORef` val))
        (lookup varname env)
  return val

defineVar :: Environment -> String -> SchemeValue -> IOThrowsError SchemeValue
defineVar envRef varname val = do
  env <- liftIO $ readIORef envRef
  case lookup varname env of
    Just _ -> setVar envRef varname val >> return val
    Nothing -> liftIO $ do
      valRef <- newIORef val
      writeIORef envRef ((varname, valRef) : env)
      return val

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
schemeCar [SchemeCons l] = return $ fst l
schemeCar [arg] = throwError $ TypeMismatch "list" arg
schemeCar args = throwError $ ArgsNumber 1 args

schemeCdr :: [SchemeValue] -> ThrowsError SchemeValue
schemeCdr [SchemeCons l] = return $ snd l
schemeCdr [arg] = throwError $ TypeMismatch "list" arg
schemeCdr args = throwError $ ArgsNumber 1 args

schemeCons :: [SchemeValue] -> ThrowsError SchemeValue
schemeCons [v1, v2] = return $ SchemeCons (v1, v2)
schemeCons args = throwError $ ArgsNumber 1 args

schemeIf :: [SchemeValue] -> ThrowsError SchemeValue
schemeIf [SchemeBool b, v1, v2] = return $ if b then v1 else v2
schemeIf [arg, _, _] = throwError $ TypeMismatch "bool" arg
schemeIf args = throwError $ ArgsNumber 3 args

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
  ("cdr", schemeCdr),
  ("cons", schemeCons),
  ("if", schemeIf)]

eval :: Environment -> Expr -> IOThrowsError SchemeValue
eval _ (NumberExpr x) = return $ SchemeNumber x
eval _ (BoolExpr b) = return $ SchemeBool b
eval env (ListExpr l) = fmap SchemeList (sequence $ fmap (eval env) l)
eval env (ConsExpr (l, r)) = do
  left <- eval env l
  right <- eval env r
  return $ SchemeCons (left, right)
eval env (SymbolExpr varname) = getVar env varname
eval _ NilExpr = return SchemeNil
eval env (ReservedOpCallExpr op args) = eval env args >>= liftThrows . unwrapList >>= liftThrows . fromJust (lookup op opMap)
eval env (DefineVarExpr varname expr) = eval env expr >>= defineVar env varname
