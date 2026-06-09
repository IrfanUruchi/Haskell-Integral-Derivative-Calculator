module Eval (eval) where
import Expr

eval :: Expr -> Double -> Double
eval (Num n) _ = n
eval Var x = x
eval (Add a b) x = eval a x + eval b x
eval (Sub a b) x = eval a x - eval b x
eval (Mul a b) x = eval a x * eval b x
eval (Div a b) x = eval a x / eval b x
eval (Pow a b) x = eval a x ** eval b x
eval (Sin e) x   = sin  (eval e x)
eval (Cos e) x   = cos  (eval e x)
eval (Tan e) x   = tan  (eval e x)
eval (Atan e) x  = atan (eval e x)
eval (Exp e) x   = exp  (eval e x)
eval (Log e) x   = log  (eval e x)
eval (Sinh e) x  = sinh (eval e x)
eval (Cosh e) x  = cosh (eval e x)
eval (Tanh e) x  = tanh (eval e x)
eval (Neg e) x   = negate (eval e x)
