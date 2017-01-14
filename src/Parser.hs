module Parser where

import Text.Parsec hiding (string, spaces)
import Text.Parsec.String (Parser)

import AST
import Lexer

{-# ANN module "HLint: ignore Reduce duplication" #-}

parseNumber :: Parser Expr
parseNumber = fmap NumberExpr number

parseBool :: Parser Expr
parseBool = fmap BoolExpr bool

parseChar :: Parser Expr
parseChar = fmap CharExpr char_

parseList :: Parser Expr
parseList = ListExpr <$> sepBy parseExpr spaces

parseString :: Parser Expr
parseString =  fmap (AST.fromList . (++ [NilExpr]) . fmap CharExpr) string

parseSymbol :: Parser Expr
parseSymbol = fmap SymbolExpr symbol

parseQuoted :: Parser Expr
parseQuoted = do
  reserved "'"
  char '('
  cars <- sepBy parseExpr spaces
  cdr <- optionMaybe (char '.' >> spaces >> parseExpr)
  char ')'
  return $ case cdr of
    Nothing -> AST.fromList (cars `mappend` [NilExpr])
    Just e -> AST.fromList (cars `mappend` [e])

parseIf :: Parser Expr
parseIf = do
  char '('
  reserved "if"
  spaces
  cond <- parseExpr
  spaces
  left <- parseExpr
  spaces
  right <- parseExpr
  spaces
  char ')'
  return $ IfExpr cond left right

parseFuncCall :: Parser Expr
parseFuncCall = do
  char '('
  caller <- parseExpr
  spaces
  args <- parseList
  char ')'
  return $ FuncCallExpr caller args

parseDefine :: Parser Expr
parseDefine = do
  char '('
  reserved "define"
  spaces
  varname <- symbol
  expr <- parseExpr
  char ')'
  return $ DefineVarExpr varname expr

parseSet :: Parser Expr
parseSet = do
  char '('
  reserved "set!"
  spaces
  varname <- symbol
  expr <- parseExpr
  char ')'
  return $ SetVarExpr varname expr

parseLambda :: Parser Expr
parseLambda = do
  char '('
  reserved "lambda"
  spaces
  args <- parens (symbol `sepBy` spaces)
  spaces
  body <- parseExpr
  char ')'
  return $ LambdaFuncExpr args body

parseFuncDefine :: Parser Expr
parseFuncDefine = do
  char '('
  reserved "define"
  spaces
  args <- parens (symbol `sepBy` spaces)
  spaces
  body <- parseExpr
  char ')'
  return $ DefineVarExpr (head args) (LambdaFuncExpr (tail args) body)

parseBegin :: Parser Expr
parseBegin = do
  char '('
  reserved "begin"
  spaces
  args <- parseList
  char ')'
  return $ BeginExpr args

parseWhile :: Parser Expr
parseWhile = do
  char '('
  reserved "while"
  spaces
  cond <- parseExpr
  spaces
  body <- parseExpr
  char ')'
  return . BeginExpr $ ListExpr [
    DefineVarExpr "`whilerec" (
      LambdaFuncExpr [] (
        IfExpr cond (BeginExpr $ ListExpr [
          body,
          FuncCallExpr (SymbolExpr "`whilerec") (ListExpr [])
        ]) cond)
    ),
    FuncCallExpr (SymbolExpr "`whilerec") (ListExpr [])]

parseMakeVector :: Parser Expr
parseMakeVector = do
  char '('
  reserved "make-vector"
  spaces
  varname <- symbol
  spaces
  length <- parseExpr
  char ')'
  return $ DefineVarExpr varname (VectorInitExpr length)

parseVectorSet :: Parser Expr
parseVectorSet = do
  char '('
  reserved "vector-set!"
  spaces
  a <- symbol
  spaces
  idx <- parseExpr
  spaces
  val <- parseExpr
  char ')'
  return $ SetVarExpr a (UpdateVectorExpr (SymbolExpr a) idx val)

parseExpr :: Parser Expr
parseExpr = do
  spaces
  expr <- parseNumber
      <|> parseBool
      <|> parseChar
      <|> parseQuoted
      <|> parseSymbol
      <|> parseString
      <|> try parseIf
      <|> try parseMakeVector
      <|> try parseVectorSet
      <|> try parseWhile
      <|> try parseLambda
      <|> try parseSet
      <|> try parseDefine
      <|> try parseFuncDefine
      <|> try parseBegin
      <|> parseFuncCall
  spaces
  return expr
