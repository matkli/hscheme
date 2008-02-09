-- Expr: Scheme expression module
--
-- Copyright (C) 2008 Mats Klingberg

module Expr ( Expr(..), isList, listToPairs, pairsToList, testExpr ) where

import Types

instance Show Expr where
    show (Symbol x) = x
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

-- Check if a pair is a proper list (i.e. Null terminated)
isList :: Expr -> Bool
isList Null = True
isList (Pair a b) = isList b
isList _ = False

-- Convert a haskell list to a Scheme list, i.e. pairs
listToPairs :: [Expr] -> Expr
listToPairs [] = Null
listToPairs (x:xs) = Pair x (listToPairs xs)

-- Convert a Scheme list to a Haskell list
pairsToList :: Expr -> [Expr]
pairsToList Null = []
pairsToList (Pair a b) = a:(pairsToList b)

-- Test expressions
testExpr :: [Expr]
testExpr = 
    [Pair (Number 1) (Pair (Number 2) Null),
     Pair (String "asdf") (Pair (Bool True) (Bool False)),
     Symbol "atom",
     Null]

