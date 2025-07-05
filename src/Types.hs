module Types where

data Expr = 
  Constant Double |
  Var (Char,Int)  | 
  Add Expr Expr   |
  Sub Expr Expr   |
  Mul Expr Expr   |
  Div Expr Expr   |
  Pow Expr Expr 
  deriving (Eq,Show)

type Equation = (Expr,Expr)

data ExprType = Linear | Polyinomial Int Int | Other


