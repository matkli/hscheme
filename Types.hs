-- Types: Common type declarations to avoid mutually recursive modules
--
-- Copyright (C) 2008 Mats Klingberg

module Types ( Expr(..), SchemeError(..), PrimitiveFunction, ThrowsError ) where

import Text.ParserCombinators.Parsec ( ParseError )

-- Type for storing a scheme expression
data Expr = Symbol String           -- Scheme symbol
          | Number Integer          -- We only support integers so far       
          | Bool Bool               -- Booleans
          | String String           -- String ("asdf")
          | Pair Expr Expr          -- Basic list building block
          | Null                    -- Empty list
          | PrimFunc String PrimitiveFunction -- Primitive function

-- Datatype for scheme functions
type PrimitiveFunction = [Expr] -> ThrowsError Expr

-- Data type for storing errors
data SchemeError = NumArgs Integer [Expr]
                 | TypeError String Expr
                 | ParseError ParseError
                 | BadSpecialForm String Expr
                 | NotFunction Expr
                 | UnboundVar String
                 | Default String

-- Convenience type for functions that can throw a SchemeError
type ThrowsError = Either SchemeError

