-- | Primitive function definitions for HScheme.
--
-- Copyright 2008 Mats Klingberg
--
-- This file is part of hscheme and is licensed under the GNU GPL, see the
-- LICENSE file for the full license text.

module Primitives (
    -- * Exported Haskell functions
    getPrimitiveEnv

    -- * Primitive Scheme functions

    -- ** Numerical operations
    -- $numops

    -- ** Numerical comparisons
    -- $numcomp

    -- ** List operations
    -- $listops
  ) where

-- System imports
import Data.List
import Control.Monad.Except
import Data.IORef

-- Local imports
import Types

-- $numops
-- The following binary numerical operations are supported:
--
-- [@+@] addition
--
-- [@-@] subtraction
--
-- [@*@] multiplication
--
-- [@quotient@] Integer division

-- $numcomp
-- The basic primitive funcitons for comparing numbers (all are binary):
--
-- [@=@] Test for numeric equality
--
-- [@<@] Less than
--
-- [@>@] Greater than
--
-- [@<=@] Less than or equal
--
-- [@>=@] Greater than or equal

-- $listops
-- Basic list primitives:
--
-- [@car@] Return the first element of a pair (or the head of a list).
--
-- [@cdr@] Return the second element of a pair (or the tail of a list).
--
-- [@cons@] Create a new pair from two elements. If the second element is a
-- pair itself, the new structure is called a list.


-- | List of primitive functions
primitives :: [(String, PrimitiveFunction)]
primitives = [
    ("+", numericFoldOp (+) 0),
    ("-", minus),
    ("*", numericFoldOp (*) 1),
    ("quotient", numericBinOp quot),
    ("=", numericCompare (==)),
    ("<", numericCompare (<)),
    (">", numericCompare (>)),
    ("<=", numericCompare (<=)),
    (">=", numericCompare (>=)),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons)
  ]


-- | Get an environment with primitive functions defined.
getPrimitiveEnv :: IO Env
getPrimitiveEnv = 
    mapM addBinding primitives >>= newIORef
    where addBinding (name, func) = do f <- newIORef $ PrimFunc name func
                                       return (name, f)

-----------------------
-- Numerical operations
-----------------------

-- | Get a number (or throw an error)
getNumber :: Expr -> IOThrowsError Integer
getNumber (Number x) = return x
getNumber notNumber = throwError $ TypeError "Integer" notNumber

-- | Create numerical fold operators
numericFoldOp :: (Integer -> Integer -> Integer) -> Integer -> PrimitiveFunction
numericFoldOp func start args = fmap (Number . foldl' func start) (mapM getNumber args)

-- | Create numerical binary operators
numericBinOp :: (Integer -> Integer -> Integer) -> PrimitiveFunction
numericBinOp func (a:b:[]) = liftM2 (\x y -> Number $ func x y) (getNumber a) (getNumber b)
numericBinOp _ args = throwError $ NumArgs 2 args

-- | Subtraction
--
-- This is a little special since a unary minus should negate it's argument,
-- while binary (or m-ary) minus should subtract _from_ it's first argument.
minus :: PrimitiveFunction
minus [] = throwError $ NumArgs 1 []
minus (x:[]) = fmap (Number . negate) (getNumber x)
minus (x:xs) = getNumber x >>= \num -> numericFoldOp (-) num xs

-- | Numerical comparisons
numericCompare :: (Integer -> Integer -> Bool) -> PrimitiveFunction
numericCompare func args =
    do nums <- mapM getNumber args
       return $ Bool $ foldr (\(x,y) -> (&& func x y)) 
                                        True 
                                        (zip nums $ tail nums)

------------------
-- List operations
------------------

-- | First element of list or pair
car :: PrimitiveFunction
car [List (x:xs)] = return x
car [Dotted (x:xs) _] = return x
car [List []] = throwError $ TypeError "List" $ List []
car [noList] = throwError $ TypeError "List" noList
car args = throwError $ NumArgs 1 args

-- | Tail of list or second element of pair
cdr :: PrimitiveFunction
cdr [List (x:xs)] = return $ List xs
cdr [Dotted [x] y] = return y
cdr [Dotted (x:xs) y] = return $ Dotted xs y
cdr [List []] = throwError $ TypeError "List" $ List []
cdr [noList] = throwError $ TypeError "List" noList
cdr args = throwError $ NumArgs 1 args

-- | Create a pair or add a new head to list
cons :: PrimitiveFunction
cons [x, List xs] = return $ List (x:xs)
cons [x, Dotted xs y] = return $ Dotted (x:xs) y
cons [x, y] = return $ Dotted [x] y
cons args = throwError $ NumArgs 2 args
