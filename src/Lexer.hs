module Lexer where

import Text.Parsec hiding (string, spaces)
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = let
  names = ["\'", "lambda", "define", "begin", "while", "let", "if", "set!", "#t", "#f"]
  symbolLetter = oneOf "!#$%&|*+-/:<=>?@^_~"
  def = emptyDef {
    Tok.commentStart = "#|",
    Tok.commentEnd = "|#",
    Tok.commentLine = ";",
    Tok.nestedComments = False,
    Tok.identStart = symbolLetter <|> letter,
    Tok.identLetter = symbolLetter <|> digit <|> letter,
    Tok.reservedOpNames = [],
    Tok.reservedNames = names,
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
spaces = skipMany space -- TODO: buggy, skipMany1
