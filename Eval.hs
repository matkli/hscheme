-- Eval: Eavluate scheme expressions
--
-- Copyright (C) 2008 Mats Klingberg

module Eval( eval ) where

-- Standard imports

-- Local imports
import Expr

-- eval
-- Evaluate a scheme expression
eval :: Expr -> Expr
eval val@(Number _) = val       -- Number evaluate to themselves
eval val@(Bool _) = val         -- Booleans evaluate to themselves
eval val@(String _) = val       -- Strings evaluate to themselves
--eval (Pair (Symbol "quote") (Pair expr Null)) = expr -- Quoted expressions
eval e@(Pair _ _)
    | isList e = evalList $ pairsToList e
    | otherwise = error "Illegal expression"
eval _ = error "Illegal expression"

-- Evaluate a list
evalList :: [Expr] -> Expr
evalList ((Symbol "quote"):expr:[]) = expr
evalList _ = error "Illegal expression"

