module AST where

data Expr =
  NumberExpr Double |
  BoolExpr Bool |
  ListExpr [Expr] |
  ReservedOpCallExpr String Expr deriving (Show)
