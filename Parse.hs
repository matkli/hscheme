-- Parse: Scheme parser module
--
-- Copyright (C) 2008 Mats Klingberg

module Parse ( readExpr, identifier, boolean ) where

-- Standard imports
import Text.ParserCombinators.Parsec
import Char

-- Local imports
import Expr

-- Numerical constants
-- Only integers so far
number :: Parser Expr
number = do sign <- option '+' $ oneOf "+-"
            rest <- many1 digit
            return $ Number $ if sign == '+' 
                                 -- read doesn't lide leading '+'
                                 then read rest
                                 else read (sign:rest)

-- Boolean constants
boolean :: Parser Expr
boolean = do c <- char '#' >> oneOf "tTfF"
             return $ case (toLower c) of
                           'f' -> Bool False
                           't' -> Bool True

-- Identifiers
identifier :: Parser Expr
identifier = 
    let initial = letter <|> oneOf "!$%&*+-./:<=>?@^_~"
    in do first <- initial
          rest <- many $ initial <|> digit
          return $ Symbol $ map toLower $ first:rest

-- Read a scheme expression and print it's representation
-- ToDo: Use "parseTest" from Parsec instead
readExpr :: String -> String
readExpr input = case parse (number <|> boolean <|> identifier) "hscheme" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val
