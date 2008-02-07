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
eval (Symbol sym) = lookupName sym
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
evalList (func:args) = apply (eval func) $ map eval args
evalList _ = error "Illegal expression"

-- Lookup a name
lookupName :: String -> Expr
lookupName name = maybe (error "Undefined variable")
                        (\fun -> PrimFunc fun) 
                        (lookup name primitives)

-- Apply a function
apply :: Expr -> [Expr] -> Expr
apply (PrimFunc func) = func
apply _ = error "Not a function"

-- List of primitive functions
primitives :: [(String, [Expr] -> Expr)]
primitives =
    [("+", numericFoldOp (+)),
     ("-", numericFoldOp (-)),
     ("*", numericFoldOp (*)),
     ("quotient", numericBinOp quot)]

-- Create numerical fold operators
numericFoldOp :: (Integer -> Integer -> Integer) -> [Expr] -> Expr
numericFoldOp func args = Number $ foldl1 func $ map getNum args 
    where getNum (Number x) = x
          getNum _ = error "Expected a number"

-- Create numerical binary operators
numericBinOp :: (Integer -> Integer -> Integer) -> [Expr] -> Expr
numericBinOp func args =
    if length args == 2
       then numericFoldOp func args
       else error "Expected exactly 2 arguments"
