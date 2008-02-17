-- Parse: Scheme parser module
--
-- Copyright (C) 2008 Mats Klingberg

module Parse ( readExpr, testParser ) where

-- Standard imports
import Text.ParserCombinators.Parsec
import Control.Monad.Error
import Char

-- Local imports
import Types

-- Numerical literals
-- Only integers so far
number :: Parser Expr
number = do sign <- option '+' $ oneOf "+-"
            rest <- many1 digit
            return $ Number $ if sign == '+' 
                                 -- read doesn't like leading '+'
                                 then read rest
                                 else read (sign:rest)

-- Boolean literals
boolean :: Parser Expr
boolean = do c <- char '#' >> oneOf "tTfF"
             return $ case (toLower c) of
                           'f' -> Bool False
                           't' -> Bool True

-- Identifiers
-- This is not quite R5RS compliant
identifier :: Parser Expr
identifier = 
    let initial = letter <|> oneOf "!$%&*+-/:<=>?@^_~"
    in do first <- initial
          rest <- many $ initial <|> digit
          return $ Symbol $ map toLower $ first:rest

-- Strings
string_ :: Parser Expr
string_ = do char '\"'
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
                           _ -> fail "Illegal escape sequence" 

-- Quoted expressions
quoted :: Parser Expr
quoted = do char '\'' >> spaces
            e <- expr
            return $ List [Symbol "quote", e]

-- List
-- This handles both proper lists and dotted lists (pairs)
list :: Parser Expr
list = do char '(' >> spaces
          lst <- expr `sepBy` spaces 
          proper lst <|> dotted lst
    where proper lst = char ')' >> (return $ List lst)
          dotted lst = do char '.' >> spaces
                          cdr <- expr
                          spaces >> char ')'
                          return $ Dotted lst cdr

-- Any parser followed by optional space
lexeme ::  GenParser Char st t -> GenParser Char st t
lexeme parser = do p <- parser
                   spaces
                   return p

-- Parse a complete expression
expr :: Parser Expr
expr = (lexeme $ (try number)
              <|> boolean
              <|> string_
              <|> identifier
              <|> quoted
              <|> list)

-- Read a scheme expression and print it's representation
readExpr :: String -> ThrowsError Expr
readExpr str = case parse (spaces >> expr) "" str of
                    Left err -> throwError $ ParseError err
                    Right val -> return val

-- Parser test
testParser :: [ThrowsError Expr]
testParser = map readExpr testExpressions

testExpressions :: [String]
testExpressions = 
    ["+47",
     "()",
     "\"String with \\\"escapes\\\":\\n\\tgoes\\n\\there\"",
     " ( oddly  spaCed ( expreSSion ) ) ",
     "(this is a dotted . list)",
     unlines ["((numbers +47 -47 0047)", 
              " (booleans #t #f #T #F)",
              " (strings \"Test\" \"asdf\")",
              " (symbols a b c))"],
     "(parse error #a)"]
