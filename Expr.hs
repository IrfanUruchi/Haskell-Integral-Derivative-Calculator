module Expr where

data Expr
  = Num Double
  | Var
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  | Sin Expr
  | Cos Expr
  | Tan Expr
  | Atan Expr
  | Exp Expr
  | Log Expr
  | Sinh Expr
  | Cosh Expr
  | Tanh Expr
  | Neg Expr
  deriving (Eq, Show, Read)
