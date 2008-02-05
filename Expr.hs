-- Expr: Scheme expression module
--
-- Copyright (C) 2008 Mats Klingberg

module Expr (Expr(..)) where

data Expr = Symbol String           -- Scheme symbol
          | Number Integer          -- We only support integers so far       
          | Bool Bool               -- Booleans
          | String String           -- String ("asdf")
          | Pair Expr Expr          -- Basic list building block
          | Null                    -- Empty list

instance Show Expr where
    show (Symbol x) = show x
    show (Number x) = show x
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (String str) = "\"" ++ str ++ "\""
    show p@(Pair _ _) = showPair p
    show Null = "()"

-- Show a pair
-- This is a little tricky, since a pair with a pair in cdr should be shown in
-- "list"-style, i.e. without the dot and without extra parentheses.
showPair :: Expr -> String
showPair p@(Pair _ _) = "(" ++ showPairNoParen p ++ ")" where
    showPairNoParen :: Expr -> String
    showPairNoParen (Pair car Null) = show car
    showPairNoParen (Pair car cdr@(Pair _ _)) = show car ++ " " ++ showPairNoParen cdr
    showPairNoParen (Pair car cdr) = show car ++ " . " ++ show cdr

