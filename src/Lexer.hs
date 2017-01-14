module Lexer where

import Text.Parsec hiding (string, spaces)
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

reservedNames = ["\'", "lambda", "define",
                 "begin", "while", "if",
                 "let", "set!",
                 "make-vector", "vector-set!",
                 "#t", "#f"]

lexer :: Tok.TokenParser ()
lexer = let
  symbolLetter = oneOf "!#$%&|*+-/:<=>?@^_~"
  def = emptyDef {
    Tok.commentStart = "#|",
    Tok.commentEnd = "|#",
    Tok.commentLine = ";",
    Tok.nestedComments = False,
    Tok.identStart = symbolLetter <|> letter,
    Tok.identLetter = symbolLetter <|> digit <|> letter,
    Tok.reservedOpNames = [],
    Tok.reservedNames = reservedNames,
    Tok.caseSensitive = True
  }
  in Tok.makeTokenParser def

number :: Parser Double
number = do
  num <- Tok.naturalOrFloat lexer
  return $ case num of
    Left int -> fromInteger int
    Right db -> db

bool :: Parser Bool
bool = True <$ reserved "#t"
   <|> False <$ reserved "#f"

symbol :: Parser String
symbol = Tok.identifier lexer

char_ :: Parser Char
char_ = do
  char '\''
  c <- noneOf "\'"
  char '\''
  return c

string :: Parser String
string = do
  char '"'
  s <- many (noneOf "\"")
  char '"'
  return s

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

spaces :: Parser ()
spaces = skipMany (oneOf " \n\t\b") -- TODO: buggy, skipMany1
