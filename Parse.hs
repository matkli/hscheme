-- Parse: Scheme parser module
--
-- Copyright (C) 2008 Mats Klingberg

module Parse (identifier, boolean) where

-- Standard imports
import Text.ParserCombinators.Parsec
import Char

-- Local imports
import Expr

-- Identifiers
identifier :: Parser Expr
identifier = 
    let initial = letter <|> oneOf "!$%&*+-./:<=>?@^_~"
    in do first <- initial
          rest <- many $ initial <|> digit
          return $ Symbol $ map toLower $ first:rest

-- Boolean constants
boolean :: Parser Expr
boolean = do c <- char '#' >> oneOf "tTfF"
             return $ case (toLower c) of
                           'f' -> Bool False
                           't' -> Bool True




