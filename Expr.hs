-- Expr: Scheme expression module
--
-- Copyright (C) 2008 Mats Klingberg

module Expr ( Expr(..), testShow, isList, listToPairs, pairsToList ) where

data Expr = Symbol String           -- Scheme symbol
          | Number Integer          -- We only support integers so far       
          | Bool Bool               -- Booleans
          | String String           -- String ("asdf")
          | Pair Expr Expr          -- Basic list building block
          | Null                    -- Empty list

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

-- Test the show functionality
testShow :: IO ()
testShow = let e1 = Pair (Number 1) (Pair (Number 2) Null)
               e2 = Pair (String "asdf") (Pair (Bool True) (Bool False))
               e3 = Symbol "atom"
               e4 = Null in
           do putStrLn $ show e1
              putStrLn $ show e2
              putStrLn $ show e3
              putStrLn $ show e4

