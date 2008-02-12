-- Types: Common type declarations to avoid mutually recursive modules
--
-- Copyright (C) 2008 Mats Klingberg

module Types ( Expr(..), 
               SchemeError(..), 
               PrimitiveFunction, 
               Env,
               ThrowsError, 
               IOThrowsError )
             where

import Text.ParserCombinators.Parsec ( ParseError )
import Control.Monad.Error
import Data.IORef

-- Type for storing a scheme expression
data Expr = Symbol String           -- Scheme symbol
          | Number Integer          -- We only support integers so far       
          | Bool Bool               -- Booleans
          | String String           -- String ("asdf")
          | Pair Expr Expr          -- Basic list building block
          | Null                    -- Empty list
          | PrimFunc String PrimitiveFunction -- Primitive function
          | Undefined               -- Return from e.g. set

-- Datatype for scheme functions
type PrimitiveFunction = [Expr] -> IOThrowsError Expr

-- Data type for storing errors
data SchemeError = NumArgs Integer [Expr]
                 | TypeError String Expr
                 | ParseError ParseError
                 | BadSpecialForm String Expr
                 | NotFunction Expr
                 | UnboundVar String
                 | Default String

-- An environment type (stores variable bindings)
type Env = IORef [(String, IORef Expr)]

-- Convenience type for functions that can throw a SchemeError
type ThrowsError = Either SchemeError
type IOThrowsError = ErrorT SchemeError IO

