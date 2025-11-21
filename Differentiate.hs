module Differentiate (differentiateSymbolic) where
import Expr

differentiateSymbolic :: Expr -> Expr
differentiateSymbolic (Num _) = Num 0
differentiateSymbolic Var     = Num 1

differentiateSymbolic (Add a b) = Add (differentiateSymbolic a) (differentiateSymbolic b)
differentiateSymbolic (Sub a b) = Sub (differentiateSymbolic a) (differentiateSymbolic b)

differentiateSymbolic (Mul a b) =
  Add (Mul (differentiateSymbolic a) b)
      (Mul a (differentiateSymbolic b))

differentiateSymbolic (Div a b) =
  Div (Sub (Mul (differentiateSymbolic a) b)
           (Mul a (differentiateSymbolic b)))
      (Pow b (Num 2))

-- u^n
differentiateSymbolic (Pow a (Num n)) =
  Mul (Mul (Num n) (Pow a (Num (n - 1))))
      (differentiateSymbolic a)

-- u^v general case
differentiateSymbolic (Pow u v) =
  let u' = differentiateSymbolic u
      v' = differentiateSymbolic v
  in Mul (Pow u v) (Add (Mul v' (Log u)) (Mul v (Div u' u)))

differentiateSymbolic (Sin e)  = Mul (Cos e) (differentiateSymbolic e)
differentiateSymbolic (Cos e)  = Mul (Num (-1)) (Mul (Sin e) (differentiateSymbolic e))
differentiateSymbolic (Tan e)  = Mul (Div (Num 1) (Pow (Cos e) (Num 2))) (differentiateSymbolic e)
differentiateSymbolic (Atan e) = Div (differentiateSymbolic e) (Add (Num 1) (Pow e (Num 2)))
differentiateSymbolic (Exp e)  = Mul (Exp e) (differentiateSymbolic e)
differentiateSymbolic (Log e)  = Div (differentiateSymbolic e) e
differentiateSymbolic (Sinh e) = Mul (Cosh e) (differentiateSymbolic e)
differentiateSymbolic (Cosh e) = Mul (Sinh e) (differentiateSymbolic e)
differentiateSymbolic (Tanh e) = Mul (Sub (Num 1) (Pow (Tanh e) (Num 2))) (differentiateSymbolic e)
differentiateSymbolic (Neg e)  = Neg (differentiateSymbolic e)

