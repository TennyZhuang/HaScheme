module Lexer where

import Text.Parsec hiding (string, spaces)
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Token as Tok

ops = ["+", "-", "*", "/",
       "<", ">", "=",
       "&&", "||", "not",
       "car", "cdr", "cons"]

lexer :: Tok.TokenParser ()
lexer = let
  names = ["\'", "lambda", "define", "let", "set!", "#t", "#f"]
  symbolLetter = oneOf "!#$%&|*+-/:<=>?@^_~"
  def = Tok.LanguageDef {
    Tok.commentStart = "#|",
    Tok.commentEnd = "|#",
    Tok.commentLine = ";",
    Tok.nestedComments = False,
    Tok.identStart = symbolLetter <|> letter,
    Tok.identLetter = symbolLetter <|> digit <|> letter,
    Tok.opStart = oneOf $ fmap head ops,
    Tok.opLetter = oneOf $ concat ops,
    Tok.reservedOpNames = ops,
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

reservedOp :: Parser String
reservedOp = choice $ fmap (\op -> fmap (const op) (Tok.reservedOp lexer op)) ops

parens :: Parser a -> Parser a
parens = Tok.parens lexer

spaces :: Parser ()
spaces = skipMany space -- TODO: buggy, skipMany1
