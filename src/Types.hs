module Types where

data Expr = 
    Constant Double
  | Var (Char,Int)
  | Add [Expr]
  | Neg Expr
  | Mul [Expr]
  | Inv Expr
  | Pow Expr Expr
  deriving (Eq,Show)

add (Add a) (Add b) = Add (a++b)
add (Add a) b = Add (a ++ [b])
add a (Add b) = Add (a:b)
add a b = Add [a,b]

mul (Mul a) (Mul b) = Mul (a++b)
mul (Mul a) b = Mul (a ++ [b])
mul a (Mul b) = Mul (a:b)
mul a b = Mul [a,b]

sub a b = add a $ Neg b
divide a b = mul a $ Inv b  

type Equation = (Expr,Expr)

data ExprType = Linear | Polyinomial Int Int | Other


