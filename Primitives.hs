-- Primitives: Primitive function definitions for HScheme
--
-- Copyright (C) 2008 Mats Klingberg

module Primitives ( getPrimitiveEnv ) where

-- System imports
import Data.List
import Control.Monad
import Control.Monad.Error
import Data.IORef

-- Local imports
import Types
import Error


-- List of primitive functions
primitives :: [(String, PrimitiveFunction)]
primitives =
    [("+", numericFoldOp (+) 0),
     ("*", numericFoldOp (*) 1),
     ("-", minus),
     ("quotient", numericBinOp quot)]


-- Get an environment with primitive functions defined
getPrimitiveEnv :: IO Env
getPrimitiveEnv = 
    mapM addBinding primitives >>= newIORef
    where addBinding (name, func) = do f <- newIORef $ PrimFunc name func
                                       return (name, f)

-- Create numerical fold operators
numericFoldOp :: (Integer -> Integer -> Integer) -> Integer -> PrimitiveFunction
numericFoldOp func id args = (mapM getNumber args) >>= return . Number . foldl' func id 

-- Create numerical binary operators
numericBinOp :: (Integer -> Integer -> Integer) -> PrimitiveFunction
numericBinOp func (a:b:[]) = liftM2 (\a b -> Number $ func a b) (getNumber a) (getNumber b)
numericBinOp func args = throwError $ NumArgs 2 args

-- Minus
-- This is a little special since a unary minus should negate it's argument,
-- while binary (or m-ary) minus should subtract _from_ it's first argument.
minus :: PrimitiveFunction
minus [] = throwError $ NumArgs 1 []
minus (x:[]) = getNumber x >>= return . Number . negate
minus (x:xs) = getNumber x >>= \x -> numericFoldOp (-) x xs

-- Get a number (or throw an error)
getNumber :: Expr -> IOThrowsError Integer
getNumber (Number x) = return x
getNumber notNumber = throwError $ TypeError "Integer" notNumber

