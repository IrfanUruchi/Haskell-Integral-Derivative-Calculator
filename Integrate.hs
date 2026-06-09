module Integrate
  ( integrateNumeric
  , integrateNumericAdaptive
  , integrateSymbolic
  , integrateDefiniteAuto
  ) where

import Expr
import Eval      (eval)
import Simplify  (simplify)
import GHC.Float (isNaN, isInfinite)

integrateNumeric :: (Double -> Double) -> Double -> Double -> Int -> Double
integrateNumeric f a b n
  | n <= 0    = error "n must be positive"
  | odd n     = error "Simpson's rule requires an even number of intervals"
  | otherwise =
      let h     = (b - a) / fromIntegral n
          step i acc
            | i > n    = acc * h / 3
            | otherwise =
                let x  = a + fromIntegral i * h
                    w  | i == 0 || i == n = 1.0 :: Double
                       | even i           = 2.0
                       | otherwise        = 4.0
                in step (i + 1) (acc + w * f x)
      in step 0 0

simpson :: (Double -> Double) -> Double -> Double -> Double
simpson f a b =
  let c  = (a + b) / 2
      h  = b - a
      fa = f a; fc = f c; fb = f b
      ok x = not (isNaN x) && not (isInfinite x)
  in if ok fa && ok fc && ok fb
        then (h / 6) * (fa + 4 * fc + fb)
        else 0/0  

-- Depth-capped
integrateNumericAdaptive :: (Double -> Double) -> Double -> Double -> Double -> Double
integrateNumericAdaptive f a b eps =
  let whole = simpson f a b
  in asrCapped f a b eps whole 0 25  

asrCapped :: (Double -> Double) -> Double -> Double -> Double -> Double -> Int -> Int -> Double
asrCapped f a b eps whole depth maxDepth =
  let c     = (a + b) / 2
      left  = simpson f a c
      right = simpson f c b
      err   = left + right - whole
      ok    = abs err <= 15 * eps
  in if ok
        then left + right + err / 15
        else if depth >= maxDepth
               then left + right + err / 15
               else asrCapped f a c (eps/2) left  (depth+1) maxDepth
                  + asrCapped f c b (eps/2) right (depth+1) maxDepth

integrateDefiniteAuto :: Expr -> Double -> Double -> Double -> Double
integrateDefiniteAuto e a b eps =
  let f = eval (simplify e)
  in integrateNumericAdaptive f a b eps

integrateSymbolic :: Expr -> Expr
integrateSymbolic e0 =
  let e = simplify e0
  in case antideriv e of
       Just a  -> a
       Nothing -> error ("No rule for integration of: " ++ show e)

-- Antiderivative rules 
antideriv :: Expr -> Maybe Expr
antideriv (Num c)          = Just (Mul (Num c) Var)
antideriv Var              = Just (Div (Pow Var (Num 2)) (Num 2))
antideriv (Add u v)        = Add <$> antideriv u <*> antideriv v
antideriv (Sub u v)        = Sub <$> antideriv u <*> antideriv v
antideriv (Mul (Num c) u)  = Mul (Num c) <$> antideriv u
antideriv (Mul u (Num c))  = Mul (Num c) <$> antideriv u

antideriv (Pow Var (Num n))
  | approxEq n (-1) = Just (Log Var)
  | otherwise       = Just (Div (Pow Var (Num (n+1))) (Num (n+1)))

antideriv (Div (Num c) Var) = Just (Mul (Num c) (Log Var))

antideriv (Exp u) = do
  (a,_) <- linearAB u
  if approxNe a 0 then Just (Div (Exp u) (Num a)) else Nothing

antideriv (Sin u) = do
  (a,_) <- linearAB u
  if approxNe a 0 then Just (Div (Mul (Num (-1)) (Cos u)) (Num a)) else Nothing

antideriv (Cos u) = do
  (a,_) <- linearAB u
  if approxNe a 0 then Just (Div (Sin u) (Num a)) else Nothing

antideriv (Tan u) = do
  (a,_) <- linearAB u
  if approxNe a 0 then Just (Div (Mul (Num (-1)) (Log (Cos u))) (Num a)) else Nothing

antideriv (Atan u) = do
  (a,_) <- linearAB u
  let v = u
  if approxNe a 0
    then Just (Div (Sub (Mul v (Atan v))
                        (Div (Log (Add (Num 1) (Pow v (Num 2)))) (Num 2)))
                   (Num a))
    else Nothing


-- Hyperbolic with linear inner
antideriv (Sinh u) = do
  (a,_) <- linearAB u
  if approxNe a 0 then Just (Div (Cosh u) (Num a)) else Nothing

antideriv (Cosh u) = do
  (a,_) <- linearAB u
  if approxNe a 0 then Just (Div (Sinh u) (Num a)) else Nothing

antideriv (Tanh u) = do
  (a,_) <- linearAB u
  if approxNe a 0 then Just (Div (Log (Cosh u)) (Num a)) else Nothing

antideriv (Pow (Sin u) (Num 2)) = do
  (a,_) <- linearAB u
  if approxNe a 0
    then Just $ Div (Sub (Div u (Num 2))
                         (Div (Sin (Mul (Num 2) u)) (Num 4)))
                    (Num a)
    else Nothing

-- u=ax+b:  (1/a) * ( u/2 + sin(2u)/4 )
antideriv (Pow (Cos u) (Num 2)) = do
  (a,_) <- linearAB u
  if approxNe a 0
    then Just $ Div (Add (Div u (Num 2))
                         (Div (Sin (Mul (Num 2) u)) (Num 4)))
                    (Num a)
    else Nothing


antideriv (Neg u) = Neg <$> antideriv u


antideriv _ = Nothing

approxEq :: Double -> Double -> Bool
approxEq a b = abs (a - b) < 1e-12

approxNe :: Double -> Double -> Bool
approxNe a b = not (approxEq a b)

-- u = a*x + b
linearAB :: Expr -> Maybe (Double, Double)
linearAB e = go e
  where
    go (Add (Mul (Num a) Var) (Num b)) = Just (a, b)
    go (Add (Num b) (Mul (Num a) Var)) = Just (a, b)
    go (Mul (Num a) Var)               = Just (a, 0)
    go (Add Var (Num b))               = Just (1, b)
    go (Add (Num b) Var)               = Just (1, b)
    go Var                             = Just (1, 0)
    go _                               = Nothing
