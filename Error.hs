-- Error: Module for handling interpreter errors
--
-- Copyright (C) 2008 Mats Klingberg

module Error ( SchemeError(..), ThrowsError, throwError ) where

-- System imports
import Control.Monad.Error

-- Local imports
import Types
import Expr


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
showError (ParseError err) = "Parse error: " ++ show err
showError (BadSpecialForm message expr) = message ++ ": " ++ show expr
showError (NotFunction expr) = "Expected function; found: " ++ show expr
showError (UnboundVar name) = "Variable not defined: " ++ name
showError (Default message) = "Error: " ++ message

