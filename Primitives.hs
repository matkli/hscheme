-- Primitives: Primitive function definitions for HScheme
--
-- Copyright (C) 2008 Mats Klingberg

module Primitives ( getPrimitiveEnv ) where

-- System imports
import Data.List
import Control.Monad.Error
import Data.IORef

-- Local imports
import Types


-- List of primitive functions
primitives :: [(String, PrimitiveFunction)]
primitives =
    [("+", numericFoldOp (+) 0),
     ("-", minus),
     ("*", numericFoldOp (*) 1),
     ("quotient", numericBinOp quot),
     ("=", numericCompare (==)),
     ("<", numericCompare (<)),
     (">", numericCompare (>)),
     ("<=", numericCompare (<=)),
     (">=", numericCompare (>=))]


-- Get an environment with primitive functions defined
getPrimitiveEnv :: IO Env
getPrimitiveEnv = 
    mapM addBinding primitives >>= newIORef
    where addBinding (name, func) = do f <- newIORef $ PrimFunc name func
                                       return (name, f)

-----------------------
-- Numerical operations
-----------------------

-- Get a number (or throw an error)
getNumber :: Expr -> IOThrowsError Integer
getNumber (Number x) = return x
getNumber notNumber = throwError $ TypeError "Integer" notNumber

-- Create numerical fold operators
numericFoldOp :: (Integer -> Integer -> Integer) -> Integer -> PrimitiveFunction
numericFoldOp func start args = (mapM getNumber args) >>= return . Number . foldl' func start

-- Create numerical binary operators
numericBinOp :: (Integer -> Integer -> Integer) -> PrimitiveFunction
numericBinOp func (a:b:[]) = liftM2 (\x y -> Number $ func x y) (getNumber a) (getNumber b)
numericBinOp _ args = throwError $ NumArgs 2 args

-- Minus
-- This is a little special since a unary minus should negate it's argument,
-- while binary (or m-ary) minus should subtract _from_ it's first argument.
minus :: PrimitiveFunction
minus [] = throwError $ NumArgs 1 []
minus (x:[]) = getNumber x >>= return . Number . negate
minus (x:xs) = getNumber x >>= \num -> numericFoldOp (-) num xs

-- Numerical comparisons
numericCompare :: (Integer -> Integer -> Bool) -> PrimitiveFunction
numericCompare func args =
    do nums <- mapM getNumber args
       return $ Bool $ foldr (\(x,y) -> (&& func x y)) 
                                        True 
                                        (zipWith (,) nums $ tail nums)

