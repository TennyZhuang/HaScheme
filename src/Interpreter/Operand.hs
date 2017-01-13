module Interpreter.Operand where

import Control.Monad.Except

import Interpreter.Define

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
