module Pretty (pretty) where
import Expr

formatNum :: Double -> String
formatNum n
  | fromInteger (round n :: Integer) == n = show (round n :: Integer)
  | otherwise = show n

parenIf :: (Expr -> Bool) -> Expr -> String
parenIf p e = if p e then "(" ++ pretty e ++ ")" else pretty e

isAddSub :: Expr -> Bool
isAddSub (Add _ _) = True
isAddSub (Sub _ _) = True
isAddSub _         = False


pretty :: Expr -> String
pretty (Num n)     = formatNum n
pretty Var         = "x"
pretty (Add a b)   = "(" ++ pretty a ++ " + " ++ pretty b ++ ")"
pretty (Sub a b)   = "(" ++ pretty a ++ " - " ++ pretty b ++ ")"
pretty (Mul a b)   = pretty a ++ case b of
                                   e@(Add _ _) -> "(" ++ pretty e ++ ")"
                                   e@(Sub _ _) -> "(" ++ pretty e ++ ")"
                                   e           -> pretty e
pretty (Div a b)   = "\\frac{" ++ pretty a ++ "}{" ++ pretty b ++ "}"
pretty (Pow a b)   =
  let base = case a of
               Add _ _ -> "(" ++ pretty a ++ ")"
               Sub _ _ -> "(" ++ pretty a ++ ")"
               _       -> pretty a
  in base ++ "^{" ++ pretty b ++ "}"
pretty (Sin e)     = "\\sin("   ++ pretty e ++ ")"
pretty (Cos e)     = "\\cos("   ++ pretty e ++ ")"
pretty (Tan e)     = "\\tan("   ++ pretty e ++ ")"
pretty (Atan e)    = "\\arctan("++ pretty e ++ ")"
pretty (Exp e)     = "e^{"      ++ pretty e ++ "}"
pretty (Log e)     = "\\ln("    ++ pretty e ++ ")"
pretty (Sinh e)    = "\\sinh("  ++ pretty e ++ ")"
pretty (Cosh e)    = "\\cosh("  ++ pretty e ++ ")"
pretty (Tanh e)    = "\\tanh("  ++ pretty e ++ ")"
pretty (Neg e)     = "-" ++ parenIf isAddSub e
