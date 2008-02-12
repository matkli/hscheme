-- Eval: Eavluate scheme expressions
--
-- Copyright (C) 2008 Mats Klingberg

module Eval( eval, testEval, nullEnv ) where

-- System imports
import Control.Monad.Error
import Data.IORef

-- Local imports
import Types
import Expr
import Error
import Parse (readExpr)

-- eval
-- Evaluate a scheme expression in a closure
eval :: [Env] -> Expr -> IOThrowsError Expr
eval env (Symbol sym) = getVar env sym
eval _ val@(Number _) = return val       -- Number evaluate to themselves
eval _ val@(Bool _) = return val         -- Booleans evaluate to themselves
eval _ val@(String _) = return val       -- Strings evaluate to themselves
eval env expr@(Pair _ _)
    | isList expr = evalList env (pairsToList expr)
    | otherwise = throwError $ BadSpecialForm "Illegal expression" expr
eval _ badForm = throwError $ BadSpecialForm "Illegal expression" badForm

-- Evaluate a list
evalList :: [Env] -> [Expr] -> IOThrowsError Expr
evalList _ ((Symbol "quote"):expr:[]) = return expr
evalList env (func:args) = do f <- eval env func
                              a <- mapM (eval env) args
                              apply f a
evalList _ badForm = throwError $ BadSpecialForm "Illegal expression" $ listToPairs badForm

-- Apply a function
apply :: Expr -> [Expr] -> IOThrowsError Expr
apply (PrimFunc _ func) args = func args
apply notFunc _ = throwError $ NotFunction notFunc

-- Check if a variable name is bound in an environment
isBound :: [Env] -> String -> IO Bool
isBound [] _ = return False
isBound (e:es) name = readIORef e >>= maybe (isBound es name) (const $ return True) . lookup name   

-- Lookup a name in an enviroment
getVar :: [Env] -> String -> IOThrowsError Expr
getVar [] name = throwError $ UnboundVar name
getVar (e:es) name = 
    do bindings <- liftIO $ readIORef e
       maybe (getVar es name)
             (liftIO . readIORef)
             (lookup name bindings)

-- Set the value of a variable
--setVar :: Env -> String -> IOThrowsError ()
--setvar

-- Create an empty environment
nullEnv :: IO Env
nullEnv = newIORef []

-- Test eval
testEval :: [Env] -> [IOThrowsError Expr]
testEval env = map (liftThrows . readExpr >=> eval env) testExpressions

testExpressions =                           -- Expected result
    ["(quote (this is a quoted list))",     -- (this is a quoted list)
     "(* (- 47 11) (+ 47 11))",             -- 2088
     "(quotient 8 3)",                      -- 2
     "(- 3)",                               -- -3
     "(+)",                                 -- 0
     "(*)",                                 -- 1
     "+",                                   -- #Primitive function: +
     "unboundVar",                          -- (Unbound variable error)
     "(+ 4 #t)",                            -- (Type error) 
     "(quotient 1 2 3)",                    -- (Number of arguments error)
     "(1 2 3)",                             -- (Not a funciton error)
     "#a"]                                  -- (Parse error)

