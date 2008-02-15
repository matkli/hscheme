-- Expr: Scheme expression module
--
-- Copyright (C) 2008 Mats Klingberg

module Expr ( testExpr ) where

import Types

instance Show Expr where
    show (Symbol x) = x
    show (Number x) = show x
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (String str) = "\"" ++ str ++ "\""
    show (List xs) = showListParen xs
    show (Dotted xs cdr) = showDotted xs cdr
    show (PrimFunc name _) = "#primitive " ++ name
    show (Function _ args _) = "(lambda (" ++ unwords args ++ ") ...)"
    show Undefined = "#undefined"

-- Show a list
showListParen :: [Expr] -> String
showListParen xs = "(" ++ showListNoParen xs ++ ")"

-- Show list elements
showListNoParen :: [Expr] -> String
showListNoParen = unwords . map show

-- Show a dotted list or a pair
showDotted :: [Expr] -> Expr -> String
showDotted xs cdr = "(" ++ showListNoParen xs ++ " . " ++ show cdr ++ ")"

-- Test expressions
testExpr :: [Expr]
testExpr = 
    [List [Number 1, Number 2],
     Dotted [String "asdf", Bool True] (Bool False),
     Symbol "atom",
     List []]

