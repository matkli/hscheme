-- Eval: Eavluate scheme expressions
--
-- Copyright (C) 2008 Mats Klingberg

module Eval( eval, testEval, nullEnv ) where

-- System imports
import Control.Monad.Error
import Data.IORef

-- Local imports
import Types
import Parse (readExpr)

-- eval
-- Evaluate a scheme expression in a closure
eval :: [Env] -> Expr -> IOThrowsError Expr
eval env (Symbol sym) = getVar env sym  -- lookup variable
eval _ val@(Number _) = return val      -- Number evaluate to themselves
eval _ val@(Bool _) = return val        -- Booleans evaluate to themselves
eval _ val@(String _) = return val      -- Strings evaluate to themselves
eval env (List xs) = evalList env xs
eval _ badForm = throwError $ BadSpecialForm "Illegal expression" badForm

-- Evaluate a list
evalList :: [Env] -> [Expr] -> IOThrowsError Expr
evalList _ [Symbol "quote", expr] = return expr
evalList env [Symbol "set!", Symbol name, expr] = eval env expr >>= setVar env name
evalList env [Symbol "define", Symbol name, expr] = eval env expr >>= define env name
evalList env (Symbol "define" : header : body) = defineFun env header body
evalList env [Symbol "if", test, cons, alt] = ifSyntax env test cons alt
evalList env [Symbol "if", test, cons] = ifSyntax env test cons Undefined
evalList env (Symbol "begin" : exprs) = liftM last $ mapM (eval env) exprs
evalList env (Symbol "lambda" : List args : body) = liftThrows $ lambda env args Nothing body
evalList env (Symbol "lambda" : Dotted args varargs : body) = liftThrows $ lambda env args (Just varargs) body
evalList env (func:args) = do f <- eval env func
                              a <- mapM (eval env) args
                              apply f a
evalList _ badForm = throwError $ BadSpecialForm "Illegal expression" $ List badForm

-- Apply a function
apply :: Expr -> [Expr] -> IOThrowsError Expr
apply (PrimFunc _ func) args = func args
apply (Function closure argNames vaName body) args =
    let numArgs = length args
        minArgs = length argNames
    in if numArgs < minArgs || (numArgs > minArgs && vaName == Nothing)
          then throwError $ NumArgs (toInteger minArgs) args
          else do let (a, va) = splitAt minArgs args
                  env <- liftM (:closure) $ letVars argNames a
                  maybe (return Undefined) (\s -> define env s (List va)) vaName
                  liftM last $ mapM (eval env) body  
apply notFunc _ = throwError $ NotFunction notFunc

-- Check if a variable name is bound in an environment
isBound :: [Env] -> String -> IO Bool
isBound [] _ = return False
isBound (e:es) name = readIORef e >>= maybe (isBound es name) (const $ return True) . lookup name   

-- Lookup a name and get a reference to the expression bound to that name
getRef :: [Env] -> String -> IOThrowsError (IORef Expr)
getRef [] name = throwError $ UnboundVar name
getRef (e:es) name = 
    do bindings <- liftIO $ readIORef e
       maybe (getRef es name)
             return 
             (lookup name bindings)

-- Get the value of a variable
getVar :: [Env] -> String -> IOThrowsError Expr
getVar env name = getRef env name >>= (liftIO . readIORef)

-- Set the value of a variable
setVar :: [Env] -> String -> Expr -> IOThrowsError Expr
setVar env name val = do ref <- getRef env name
                         liftIO $ writeIORef ref val
                         return Undefined

-- define
-- This will bind a variable in the innermost environment if it is not already
-- defined there. If it is defined (in the innermost environment) the behaviour
-- is equivalent to set!
define :: [Env] -> String -> Expr -> IOThrowsError Expr
define env name val = 
    do b <- liftIO $ isBound [head env] name
       if b then setVar env name val
            else liftIO $ do var <- newIORef val
                             modifyIORef (head env) ((name,var):)
                             return Undefined

-- defineFun
defineFun :: [Env] -> Expr -> [Expr] -> IOThrowsError Expr
defineFun env (List (Symbol name : args)) body =
    do val <- liftThrows $ lambda env args Nothing body
       define env name val
defineFun env (Dotted (Symbol name : args) varArgs) body = 
    do val <- liftThrows $ lambda env args (Just varArgs) body
       define env name val
defineFun _ header _ = throwError $ BadSpecialForm "Badly formed define header" $ header

-- Create a new environment with bound variables
letVars :: [String] -> [Expr] -> IOThrowsError Env
letVars names values = do vars <- liftIO $ mapM newIORef values
                          liftIO $ newIORef $ zip names vars

-- Create a function
lambda :: [Env] -> [Expr] -> (Maybe Expr) -> [Expr] -> ThrowsError Expr
lambda env args varargs [] = throwError $ BadSpecialForm "Empty function body" $ List []
lambda env args varargs body =
    do argNames <- mapM getSymbol args
       vaName <- maybe (return Nothing) (getSymbol >=> return . Just) varargs
       return $ Function env argNames vaName body
    where getSymbol (Symbol argName) = return argName
          getSymbol notSymbol = throwError $ BadSpecialForm "Formals in lambda expression must by symbols" $ notSymbol

-- if-syntax
ifSyntax :: [Env] -> Expr -> Expr -> Expr -> IOThrowsError Expr
ifSyntax env test cons alt =
    do res <- (eval env test)
       case res of
            Bool False -> eval env alt
            _ -> eval env cons

-- Create an empty environment
nullEnv :: IO Env
nullEnv = newIORef []

-- Test eval
testEval :: [Env] -> [IOThrowsError Expr]
testEval env = map (liftThrows . readExpr >=> eval env) testExpressions

testExpressions :: [String]
testExpressions =                           -- Expected result
    ["(quote (this is a quoted list))",     -- (this is a quoted list)
     "' (this is also a quoted list)",       -- (this is also a quoted list)
     "(* (- 47 11) (+ 47 11))",             -- 2088
     "(quotient 8 3)",                      -- 2
     "(- 3)",                               -- -3
     "(+)",                                 -- 0
     "(*)",                                 -- 1
     "+",                                   -- #<primitive-procedure +>
     "(< 1 2 3 4)",                         -- #t
     "(< 1 2 1 2)",                         -- #f
     "(<= 1 1 2 2 3 3)",                    -- #t
     "(= 4 4 4 4)",                         -- #t
     "(if (< 1 2) #t #f)",                  -- #t
     "(define a 4)",                        -- #undefined
     "a",                                   -- 4
     "(set! a (+ 1 1))",                    -- #undefined
     "a",                                   -- 2
     "(begin (+ 1 2) (define q 3) (- 10 q))",   -- 7
     "(lambda (x y) (+ x y))",              -- (lambda (x y) ...)
     "(define f (lambda (x y) (define a (* 2 x)) (+ a y)))",    -- #undefined
     "(define (f2 x y) (define a (* 2 x)) (+ a y))",   -- #undefined
     "(f 3 4)",                             -- 10
     "(f 1 2 3)",                           -- (Number of arguments error)
     "(f2 3 4)",                            -- 10
     "(define g (lambda (x y . z) z))",     -- #undefined
     "(g 1)",                               -- (Number of arguments error)
     "(g 1 2)",                             -- ()
     "(g 1 2 3 4 5)",                       -- (3 4 5)
     "(define p (cons 'a 'b))",             -- #undefined
     "(cons 'x p)",                         -- (x a . b)
     "(car p)",                             -- a
     "(cdr p)",                             -- b
     "(car (cdr '(1 2 3)))",                -- 2
     "(car '())",                           -- (Type error)
     "(cdr 1 2 3)",                         -- (Number of arguments error)
     "(lambda () )",                        -- (Empty body)
     "(define () (+ 3 4))",                 -- (Bad special form)
     "unboundVar",                          -- (Unbound variable error)
     "(+ 4 #t)",                            -- (Type error) 
     "(quotient 1 2 3)",                    -- (Number of arguments error)
     "(1 2 3)",                             -- (Not a funciton error)
     "#a"]                                  -- (Parse error)

