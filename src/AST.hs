module AST where

data Expr =
  NumberExpr Double |
  BoolExpr Bool |
  CharExpr Char |
  SymbolExpr String |
  ConsExpr (Expr, Expr) |
  NilExpr |
  IfExpr Expr Expr Expr |
  ListExpr [Expr] |
  FuncCallExpr Expr Expr |
  LambdaFuncExpr [String] Expr |
  DefineVarExpr String Expr |
  SetVarExpr String Expr |
  VectorInitExpr Expr |
  UpdateVectorExpr Expr Expr Expr |
  BeginExpr Expr deriving (Show)

fromList :: [Expr] -> Expr
fromList = let
  f e a = ConsExpr (e, a)
  in foldr1 f
