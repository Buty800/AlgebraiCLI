module Classifier where 
import qualified Data.Set as Set
import Types

vars :: Expr -> Set.Set (Char,Int) 
vars expr = case expr of 
   Constant _ -> Set.empty
   Var x -> Set.singleton x
   Add xs -> Set.unions $ map vars xs
   Neg x -> vars x 
   Mul xs -> Set.unions $ map vars xs
   Inv x -> vars x
   Pow x y -> Set.union (vars x) (vars y) 


isNat :: (RealFrac n) => n -> Bool
isNat n = n >= 0 && fromIntegral (floor n) == n

isConstant :: Expr -> Bool
isConstant expr = case expr of
  Constant _ -> True
  Var _ -> False
  Add xs -> all isConstant xs
  Neg x -> isConstant x
  Mul xs -> all isConstant xs
  Inv x -> isConstant x
  Pow x y -> isConstant x && isConstant y 

isPolynomial :: Expr -> Bool
isPolynomial expr = case expr of 
  Constant _ -> True
  Var _ -> True
  Add xs -> all isPolynomial xs
  Neg x -> isPolynomial x
  Mul xs -> all isPolynomial xs
  Inv x -> isConstant x    
  Pow x (Constant n) -> isPolynomial x && isNat n
{-
grad :: Expr -> Int 
grad expr = case expr of 
  Constant _ -> 0
  Var _ -> 1 
  Add x y -> max (grad x) (grad y)
  Sub x y -> max (grad x) (grad y)
  Mul x y -> grad x + grad y 
  Div x y -> grad x - grad y 
  Pow x (Constant n) -> (grad x) * (floor n)
-}

