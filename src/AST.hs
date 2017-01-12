module AST where

data Expr =
  NumberExpr Double |
  BoolExpr Bool |
  CharExpr Char |
  ConsExpr (Expr, Expr) |
  NilExpr |
  ListExpr [Expr] |
  ReservedOpCallExpr String Expr |
  DefineVarExpr String Expr deriving (Show)

fromList :: [Expr] -> Expr
fromList = let
  f e a = ConsExpr (e, a)
  in foldr1 f
