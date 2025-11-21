{-# LANGUAGE PatternGuards #-}
module Parse (parseExpr) where

import Expr
import Data.Char (isDigit, isSpace)

-- Public API
parseExpr :: String -> Either String Expr
parseExpr s = parseAddSub (filter (not . isSpace) s)

-- E := E + T | E - T | T
-- T := T * F | T / F | F
-- F := P ^ F | P              -- right-assoc ^
-- P := number | variable | const
--    | func '(' E ')' | func ATOM
--    | '(' E ')' | -P
-- ATOM := x | number | '(' E ')' | func '(' E ')' | func ATOM  (right-greedy)

parseAddSub :: String -> Either String Expr
parseAddSub s =
  case splitTop ['+', '-'] s of
    Nothing       -> parseMulDiv s
    Just (l,op,r) ->
      case (parseAddSub l, parseMulDiv r) of
        (Right a, Right b) -> Right $ if op == '+' then Add a b else Sub a b
        (Left e, _)        -> Left e
        (_, Left e)        -> Left e

------------------- * / ---------------------
parseMulDiv :: String -> Either String Expr
parseMulDiv s =
  case splitTop ['*', '/'] s of
    Nothing       -> parsePow s
    Just (l,op,r) ->
      case (parseMulDiv l, parsePow r) of
        (Right a, Right b) -> Right $ if op == '*' then Mul a b else Div a b
        (Left e, _)        -> Left e
        (_, Left e)        -> Left e

------------------- ^ (right assoc) --------
parsePow :: String -> Either String Expr
parsePow s =
  case splitTopRightAssoc '^' s of
    Nothing      -> parseImplicitMult s
    Just (l, r)  ->
      case (parseImplicitMult l, parsePow r) of
        (Right a, Right b) -> Right (Pow a b)
        (Left e, _)        -> Left e
        (_, Left e)        -> Left e

------------------- implicit mult ---------
parseImplicitMult :: String -> Either String Expr
parseImplicitMult "" = Left "empty expression"
parseImplicitMult s =
  case splitImplicit s of
    Nothing      -> parseUnit s
    Just (l, r)  ->
      case (parseUnit l, parseImplicitMult r) of
        (Right a, Right b) -> Right (Mul a b)
        (Left e, _)        -> Left e
        (_, Left e)        -> Left e

------------------- atoms / funcs ----------
parseUnit :: String -> Either String Expr
parseUnit ""   = Left "empty expression"
parseUnit "x"  = Right Var
parseUnit "pi" = Right (Num pi)
parseUnit "e"  = Right (Num (exp 1))

parseUnit s
  | Just inner <- stripOuterParens s
  = parseAddSub inner

-- Shorthands
parseUnit "sinx"    = Right (Sin  Var)
parseUnit "cosx"    = Right (Cos  Var)
parseUnit "tanx"    = Right (Tan  Var)
parseUnit "atanx"   = Right (Atan Var)
parseUnit "arctanx" = Right (Atan Var)

-- Function calls with parentheses
parseUnit s
  | isPrefixOf "sinh("   s = parseFunc Sinh  "sinh("   s
  | isPrefixOf "cosh("   s = parseFunc Cosh  "cosh("   s
  | isPrefixOf "tanh("   s = parseFunc Tanh  "tanh("   s
  | isPrefixOf "sin("    s = parseFunc Sin   "sin("    s
  | isPrefixOf "cos("    s = parseFunc Cos   "cos("    s
  | isPrefixOf "tan("    s = parseFunc Tan   "tan("    s
  | isPrefixOf "atan("   s = parseFunc Atan  "atan("   s
  | isPrefixOf "arctan(" s = parseFunc Atan  "arctan(" s
  | isPrefixOf "log("    s = parseFunc Log   "log("    s
  | isPrefixOf "ln("     s = parseFunc Log   "ln("     s
  | isPrefixOf "exp("    s = parseFunc Exp   "exp("    s
  | otherwise             = parseFuncNoParensOrNumberOrNeg s

parseFuncNoParensOrNumberOrNeg :: String -> Either String Expr
parseFuncNoParensOrNumberOrNeg s
  | Just rest <- stripBare "sinh" s   = fmap Sinh (parseAtom rest)
  | Just rest <- stripBare "cosh" s   = fmap Cosh (parseAtom rest)
  | Just rest <- stripBare "tanh" s   = fmap Tanh (parseAtom rest)
  | Just rest <- stripBare "sin"  s   = fmap Sin  (parseAtom rest)
  | Just rest <- stripBare "cos"  s   = fmap Cos  (parseAtom rest)
  | Just rest <- stripBare "tan"  s   = fmap Tan  (parseAtom rest)
  | Just rest <- stripBare "atan" s   = fmap Atan (parseAtom rest)
  | Just rest <- stripBare "arctan" s = fmap Atan (parseAtom rest)
  | isNumber s                        = Right (Num (read s))
  | otherwise                         = parseNeg s
  where
    stripBare pre str =
      case stripPrefix pre str of
        Just rest@(c:_) | c /= '(' -> Just rest
        _                          -> Nothing

-- Parse a single ATOM 
parseAtom :: String -> Either String Expr
parseAtom s =
  case splitAfterOneAtom s of
    Nothing               -> Left "expected an argument after function name"
    Just (atomStr, rest)  ->
      if null rest
        then parsePow atomStr
        else Left ("unexpected trailing input: " ++ rest)

splitAfterOneAtom :: String -> Maybe (String, String)
splitAfterOneAtom s = do
  (pstr, rest1) <- splitOneP s
  case rest1 of
    '^':rs -> do
      (fstr, rest2) <- splitF rs
      pure (pstr ++ "^" ++ fstr, rest2)
    _ -> pure (pstr, rest1)

splitF :: String -> Maybe (String, String)
splitF s = do
  (pstr, rest1) <- splitOneP s
  case rest1 of
    '^':rs -> do
      (fstr, rest2) <- splitF rs
      pure (pstr ++ "^" ++ fstr, rest2)
    _ -> pure (pstr, rest1)

splitOneP :: String -> Maybe (String, String)
splitOneP "" = Nothing
splitOneP s@(c:_)
  | c == '(' = chopBalanced s
  | c == 'x' = Just ("x", drop 1 s)
  | isDigit c =
      let (num, rest) = span (\ch -> isDigit ch || ch == '.') s
      in Just (num, rest)
  | startsFuncName s =
      let (fname, after) = takeFuncName s
      in case after of
           '(' : _ -> do
             (paren, rest) <- chopBalanced after
             Just (fname ++ paren, rest)
           _ -> do
             (a, rest) <- splitAfterOneAtom after
             Just (fname ++ a, rest)
  | otherwise = Nothing

-- Negation
parseNeg :: String -> Either String Expr
parseNeg ('-':rest) = fmap (Mul (Num (-1))) (parseUnit rest)
parseNeg _          = Left "unrecognized token"

-- helpers 

parseFunc :: (Expr -> Expr) -> String -> String -> Either String Expr
parseFunc ctor pre s =
  case stripPrefix pre s of
    Nothing   -> Left ("expected prefix " ++ pre)
    Just rest ->
      let chunkInput = if not (null pre) && last pre == '(' then '(' : rest else rest
      in case chopBalanced chunkInput of
           Just (parenExpr, junk) | null junk ->
             let inner = drop 1 (take (length parenExpr - 1) parenExpr)
             in fmap ctor (parseAddSub inner)
           Just (_, junk) -> Left ("unexpected trailing input: " ++ junk)
           Nothing        -> Left ("missing closing ')' in " ++ s)

stripOuterParens :: String -> Maybe String
stripOuterParens s =
  case s of
    '(' : _ ->
      if enclosesWhole s
        then Just (drop 1 (take (length s - 1) s))
        else Nothing
    _ -> Nothing

enclosesWhole :: String -> Bool
enclosesWhole = go 0 0
  where
    go _ _ [] = False
    go i d (c:cs)
      | i == 0 && c /= '(' = False
      | c == '('           = go (i+1) (d+1) cs
      | c == ')' =
          let d' = d-1
          in if null cs then d' == 0 else if d' <= 0 then False else go (i+1) d' cs
      | otherwise          = go (i+1) d cs


chopBalanced :: String -> Maybe (String, String)
chopBalanced "" = Nothing
chopBalanced s@(c:_)
  | c /= '(' = Nothing
  | otherwise = go 0 0 s
  where
    go :: Int -> Int -> String -> Maybe (String, String)
    go _ _ "" = Nothing
    go i d (x:xs)
      | x == '(' =
          let d' = d + 1
          in if d' == 1 && i /= 0 then Nothing else cont (i+1) d' xs
      | x == ')' =
          let d' = d - 1
          in if d' == 0
               then Just (take (i+1) s, drop (i+1) s)
               else cont (i+1) d' xs
      | otherwise = cont (i+1) d xs
    cont :: Int -> Int -> String -> Maybe (String, String)
    cont i d xs = go i d xs

splitImplicit :: String -> Maybe (String, String)
splitImplicit s =
  let n = length s
      try :: Int -> Maybe (String, String)
      try i
        | i <= 0 || i >= n = Nothing
        | otherwise =
            let (l, r) = splitAt i s
            in if endsAtom l && startsAtom r then Just (l, r) else try (i+1)
  in try 1

endsAtom :: String -> Bool
endsAtom "" = False
endsAtom s =
  case last s of
    ')' -> True
    'x' -> True
    c   -> isDigit c || isNumber s

startsAtom :: String -> Bool
startsAtom "" = False
startsAtom str@(c:_)
  | c == '('           = True
  | c == 'x'           = True
  | isDigit c          = True
  | startsFuncName str = True
  | otherwise          = False

splitTopRightAssoc :: Char -> String -> Maybe (String, String)
splitTopRightAssoc op s = go (length s - 1) 0
  where
    go :: Int -> Int -> Maybe (String, String)
    go i depth
      | i <= 0 = Nothing
      | otherwise =
          let c = s !! i
          in case c of
              ')' -> go (i-1) (depth+1)
              '(' -> go (i-1) (depth-1)
              _   -> if depth == 0 && c == op
                       then Just (take i s, drop (i+1) s)
                       else go (i-1) depth

splitTop :: [Char] -> String -> Maybe (String, Char, String)
splitTop ops s = go 0 0
  where
    n = length s
    go :: Int -> Int -> Maybe (String, Char, String)
    go i depth
      | i >= n = Nothing
      | otherwise =
          let c = s !! i
          in case c of
              '(' -> go (i+1) (depth+1)
              ')' -> go (i+1) (depth-1)
              _   -> if depth == 0 && c `elem` ops
                       then Just (take i s, c, drop (i+1) s)
                       else go (i+1) depth

stripPrefix :: String -> String -> Maybe String
stripPrefix pre s
  | isPrefixOf pre s = Just (drop (length pre) s)
  | otherwise        = Nothing

isPrefixOf :: String -> String -> Bool
isPrefixOf pre s = take (length pre) s == pre

startsFuncName :: String -> Bool
startsFuncName s =
     isPrefixOf "sinh" s || isPrefixOf "cosh" s || isPrefixOf "tanh" s
  || isPrefixOf "sin"  s || isPrefixOf "cos"  s || isPrefixOf "tan"  s
  || isPrefixOf "atan" s || isPrefixOf "arctan" s
  || isPrefixOf "log"  s || isPrefixOf "ln"   s || isPrefixOf "exp"  s

takeFuncName :: String -> (String, String)
takeFuncName s =
  case [ pre | pre <- ["sinh","cosh","tanh","arctan","atan","sin","cos","tan","log","ln","exp"]
             , isPrefixOf pre s ] of
    (pre:_) -> (pre, drop (length pre) s)
    []      -> ("", s)

isNumber :: String -> Bool
isNumber str =
  let t = case str of
            ('-':rest) -> rest
            _          -> str
  in not (null t) && all (\c -> isDigit c || c == '.') t
