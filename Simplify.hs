module Simplify (simplify) where
import Expr

simplify :: Expr -> Expr
simplify e = case e of
  Add a b -> sAdd (simplify a) (simplify b)
  Sub a b -> sSub (simplify a) (simplify b)
  Mul a b -> sMul (simplify a) (simplify b)
  Div a b -> sDiv (simplify a) (simplify b)
  Pow a b -> sPow (simplify a) (simplify b)
  Neg a   -> sNeg (simplify a)
  Sin a   -> Sin  (simplify a)
  Cos a   -> Cos  (simplify a)
  Tan a   -> Tan  (simplify a)
  Atan a  -> Atan (simplify a)
  Exp a   -> Exp  (simplify a)
  Log a   -> Log  (simplify a)
  Sinh a  -> Sinh (simplify a)
  Cosh a  -> Cosh (simplify a)
  Tanh a  -> Tanh (simplify a)
  Num n   -> Num n
  Var     -> Var

sAdd :: Expr -> Expr -> Expr
sAdd (Num 0) y           = y
sAdd x (Num 0)           = x
sAdd (Num a) (Num b)     = Num (a + b)
sAdd a b                 = Add a b

sSub :: Expr -> Expr -> Expr
sSub x (Num 0)           = x
sSub (Num a) (Num b)     = Num (a - b)
sSub a b                 = Sub a b

sMul :: Expr -> Expr -> Expr
sMul (Num 0) _           = Num 0
sMul _ (Num 0)           = Num 0
sMul (Num 1) y           = y
sMul x (Num 1)           = x
sMul (Num a) (Num b)     = Num (a * b)
sMul (Num a) (Mul (Num b) t) = sMul (Num (a*b)) t
sMul (Mul (Num a) t) (Num b) = sMul (Num (a*b)) t
sMul (Mul a b) c         = sMul a (sMul b c)
sMul a (Mul b c)         = sMul (sMul a b) c
sMul (Num (-1)) e        = sNeg e
sMul e (Num (-1))        = sNeg e
sMul a b                 = Mul a b

sDiv :: Expr -> Expr -> Expr
sDiv (Num 0) _           = Num 0
sDiv x (Num 1)           = x
sDiv (Num a) (Num b)     = Num (a / b)
sDiv a b                 = Div a b

sPow :: Expr -> Expr -> Expr
sPow _ (Num 0)           = Num 1
sPow x (Num 1)           = x
sPow (Num a) (Num b)     = Num (a ** b)
sPow a b                 = Pow a b

sNeg :: Expr -> Expr
sNeg (Num n)             = Num (-n)
sNeg (Neg e)             = e
sNeg a                   = Neg a
