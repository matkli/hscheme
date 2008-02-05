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

-- Strings
string_ :: Parser Expr
string_ = 
    do char '\"'
       s <- many $ escape <|> noneOf "\""
       char '\"'
       return $ String s
    where escape = do char '\\'
                      c <- anyChar
                      case c of
                           '\"' -> return '\"'
                           '\\' -> return '\\'
                           'v' -> return '\v'
                           'f' -> return '\f'
                           't' -> return '\t'
                           'r' -> return '\r'
                           'n' -> return '\n'
                           otherwise -> fail "Illegal escape sequence" 

-- Read a scheme expression and print it's representation
readExpr :: String -> IO ()
readExpr = parseTest $ (try number) <|> boolean <|> string_ <|> identifier

