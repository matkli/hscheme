-- Eval: Eavluate scheme expressions
--
-- Copyright (C) 2008 Mats Klingberg

module Eval( eval, testEval ) where

-- System imports
import Control.Monad

-- Local imports
import Expr
import Error
import Parse (readExpr)
import Primitives

-- eval
-- Evaluate a scheme expression
eval :: Expr -> ThrowsError Expr
eval (Symbol sym) = lookupName sym
eval val@(Number _) = return val       -- Number evaluate to themselves
eval val@(Bool _) = return val         -- Booleans evaluate to themselves
eval val@(String _) = return val       -- Strings evaluate to themselves
eval e@(Pair _ _)
    | isList e = evalList $ pairsToList e
    | otherwise = throwError $ BadSpecialForm "Illegal expression" e
eval badForm = throwError $ BadSpecialForm "Illegal expression" badForm

-- Evaluate a list
evalList :: [Expr] -> ThrowsError Expr
evalList ((Symbol "quote"):expr:[]) = return expr
evalList (func:args) = do f <- eval func
                          a <- mapM eval args
                          apply f a
evalList badForm = throwError $ BadSpecialForm "Illegal expression" $ listToPairs badForm

-- Lookup a name
lookupName :: String -> ThrowsError Expr
lookupName name = maybe (throwError $ UnboundVar name)
                        (return . PrimFunc) 
                        (lookup name primitives)

-- Apply a function
apply :: Expr -> [Expr] -> ThrowsError Expr
apply (PrimFunc func) args = func args
apply notFunc _ = throwError $ NotFunction notFunc

-- Test eval
testEval :: [ThrowsError Expr]
testEval = map (readExpr >=> eval) testExpressions

testExpressions =                           -- Expected result
    ["(quote (this is a quoted list))",     -- (this is a quoted list)
     "(* (- 47 11) (+ 47 11))",             -- 2088
     "(quotient 8 3)",                      -- 2
     "(- 3)",                               -- -3
     "(+)",                                 -- 0
     "(*)",                                 -- 1
     "unboundVar",                          -- (Unbound variable error)
     "(+ 4 #t)",                            -- (Type error) 
     "(quotient 1 2 3)",                    -- (Number of arguments error)
     "(1 2 3)",                             -- (Not a funciton error)
     "#a"]                                  -- (Parse error)

