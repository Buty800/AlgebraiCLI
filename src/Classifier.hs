module Classifier where 
import qualified Data.Set as Set
import Types

vars :: Expr -> Set.Set (Char,Int) 
vars expr = case expr of 
   Constant _ -> Set.empty
   Var x -> Set.singleton x
   Add x y -> Set.union (vars x) (vars y)
   Sub x y -> Set.union (vars x) (vars y) 
   Mul x y -> Set.union (vars x) (vars y)  
   Div x y -> Set.union (vars x) (vars y) 
   Pow x y -> Set.union (vars x) (vars y) 

simplify = id

isNat :: (RealFrac n) => n -> Bool
isNat n = n >= 0 && fromIntegral (floor n) == n

isPolynomial :: Expr -> Bool
isPolynomial expr = case expr of 
  Constant _ -> True
  Var _ -> True
  Add x y -> isPolynomial x && isPolynomial y
  Sub x y -> isPolynomial x && isPolynomial y
  Mul x y -> isPolynomial x && isPolynomial y
  Div _ (Constant _) -> True
  Div x y -> isPolynomial $ Div x (simplify y)   
  Pow x (Constant n) -> isPolynomial x && isNat n
  Pow x y -> isPolynomial $ Pow x (simplify y) 

grad :: Expr -> Int 
grad expr = case expr of 
  Constant _ -> 0
  Var _ -> 1 
  Add x y -> max (grad x) (grad y)
  Sub x y -> max (grad x) (grad y)
  Mul x y -> grad x + grad y 
  Div x y -> grad x - grad y 
  Pow x (Constant n) -> (grad x) * (floor n)


