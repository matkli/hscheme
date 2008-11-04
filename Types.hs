-- Types: Type and instance declarations for expressions and errors
--
-- Copyright (C) 2008 Mats Klingberg

module Types (
    Env,
    Expr(..),
    PrimitiveFunction,
    SchemeError(..),
    ThrowsError,
    IOThrowsError,
    showEither,
    liftThrows,
    testExpr
  ) where 

-- System imports
import Text.ParserCombinators.Parsec ( ParseError )
import Control.Monad.Error
import Data.IORef

-- An environment type (stores variable bindings)
type Env = IORef [(String, IORef Expr)]

---------------------------------------
-- Type for storing a scheme expression
---------------------------------------

data Expr = Symbol String           -- Scheme symbol
          | Number Integer          -- We only support integers so far       
          | Bool Bool               -- Booleans
          | String String           -- String ("asdf")
          | List [Expr]             -- "Proper" lists (i.e null-terminated)
          | Dotted [Expr] Expr      -- "Dotted" lists (or pairs)
          | PrimFunc String PrimitiveFunction -- Primitive function
          | Function [Env] [String] (Maybe String) [Expr]    -- Scheme function
          | Undefined               -- Return from e.g. set

instance Show Expr where show = showExpr

-- Datatype for scheme functions
type PrimitiveFunction = [Expr] -> IOThrowsError Expr

-- Show an expression
showExpr :: Expr -> String
showExpr (Symbol x) = x
showExpr (Number x) = show x
showExpr (Bool True) = "#t"
showExpr (Bool False) = "#f"
showExpr (String str) = "\"" ++ str ++ "\""
showExpr (List xs) = showListParen xs
showExpr (Dotted xs cdr) = showDotted xs cdr
showExpr (PrimFunc name _) = "#<primitive-procedure " ++ name ++ ">"
showExpr (Function _ args varargs _) = 
    "(lambda (" ++ unwords args ++ (maybe "" (" . "++) varargs) ++ ") ...)"
showExpr Undefined = "#undefined"

-- Show a list
showListParen :: [Expr] -> String
showListParen xs = "(" ++ showListNoParen xs ++ ")"

-- Show list elements
showListNoParen :: [Expr] -> String
showListNoParen = unwords . map show

-- Show a dotted list or a pair
showDotted :: [Expr] -> Expr -> String
showDotted xs cdr = "(" ++ showListNoParen xs ++ " . " ++ show cdr ++ ")"

-- Test expressions
testExpr :: [Expr]
testExpr = 
    [List [Number 1, Number 2],
     Dotted [String "asdf", Bool True] (Bool False),
     Symbol "atom",
     List []]

-------------------------------
-- Data type for storing errors
-------------------------------

data SchemeError = NumArgs Integer [Expr]
                 | TypeError String Expr
                 | ParseError ParseError
                 | BadSpecialForm String Expr
                 | NotFunction Expr
                 | UnboundVar String
                 | Default String

-- Convenience type for functions that can throw a SchemeError
type ThrowsError = Either SchemeError
type IOThrowsError = ErrorT SchemeError IO

-- Make SchemeError an Error instance
instance Error SchemeError where
    noMsg = Default "undefined error"
    strMsg = Default

-- Also make SchemeError a Show instance
instance Show SchemeError where
    show = showError

-- Show an error
showError :: SchemeError -> String
showError (NumArgs expected found) = "Error: Expected " ++ (show expected)
    ++ " args; found: " ++ (unwords $ map show found)
showError (TypeError expected found) = "Type error: Expected " ++ expected
    ++ "; found: " ++ (show found)
showError (ParseError err) = "Parse error " ++ show err
showError (BadSpecialForm message expr) = message ++ ": " ++ show expr
showError (NotFunction expr) = "Expected function; found: " ++ show expr
showError (UnboundVar name) = "Variable not defined: " ++ name
showError (Default message) = "Error: " ++ message

-- Show either an error or an expression
showEither :: ThrowsError Expr -> String
showEither (Left err) = show err
showEither (Right val) = show val

-- Lift a regular ThrowsError into IOThrowsError
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

