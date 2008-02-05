-- Parse: Scheme parser module
--
-- Copyright (C) 2008 Mats Klingberg

module Parse ( readExpr, identifier, boolean ) where

-- Standard imports
import Text.ParserCombinators.Parsec
import Char

-- Local imports
import Expr

-- Read a scheme expression and print it's representation
-- ToDo: Use "parseTest" from Parsec instead
readExpr :: String -> String
readExpr input = case parse (identifier <|> boolean) "hscheme" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

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




