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

parseReservedOpCall :: Parser Expr
parseReservedOpCall = do
  char '('
  op <- reservedOp
  spaces
  args <- parseList
  char ')'
  return $ ReservedOpCallExpr op args

parseExpr :: Parser Expr
parseExpr = parseNumber
        <|> parseBool
        <|> parseReservedOpCall
