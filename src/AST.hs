module AST where

data Expr =
  NumberExpr Double |
  ListExpr [Expr] |
  ReservedOpCallExpr String Expr deriving (Show)
