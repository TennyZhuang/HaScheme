module Parser where

import Text.Parsec hiding (string, spaces)
import Text.Parsec.String (Parser)

import AST
import Lexer

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

parseReservedOpCall :: Parser Expr
parseReservedOpCall = do
  char '('
  op <- reservedOp
  spaces
  args <- parseList
  char ')'
  return $ ReservedOpCallExpr op args

parseDefine :: Parser Expr
parseDefine = do
  char '('
  reserved "define"
  spaces
  varname <- symbol
  expr <- parseExpr
  char ')'
  return $ DefineVarExpr varname expr

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

parseExpr :: Parser Expr
parseExpr = parseNumber
        <|> parseBool
        <|> parseQuoted
        <|> parseSymbol
        <|> try parseLambda
        <|> try parseDefine
        <|> parseReservedOpCall
