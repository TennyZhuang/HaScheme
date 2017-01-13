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

parseList :: Parser Expr
parseList = ListExpr <$> sepBy parseExpr spaces

parseString :: Parser Expr
parseString =  fmap (AST.fromList . fmap CharExpr) string

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

parseExpr :: Parser Expr
parseExpr = parseNumber
        <|> parseBool
        <|> parseQuoted
        <|> parseSymbol
        <|> try parseIf
        <|> try parseLambda
        <|> try parseSet
        <|> try parseDefine
        <|> try parseFuncDefine
        <|> parseFuncCall
