-- Parse: Scheme parser module
--
-- Copyright (C) 2008 Mats Klingberg

module Parse ( readExpr, testParser ) where

-- Standard imports
import Text.ParserCombinators.Parsec
import Char

-- Local imports
import Expr
import Error

-- Numerical literals
-- Only integers so far
number :: Parser Expr
number = do sign <- option '+' $ oneOf "+-"
            rest <- many1 digit
            return $ Number $ if sign == '+' 
                                 -- read doesn't lide leading '+'
                                 then read rest
                                 else read (sign:rest)

-- Boolean literals
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
                           otherwise -> fail "Illegal escape sequence" 

-- Atoms, i.e. an identifier or a literal of some sort
atom :: Parser Expr
atom = spaces >> ((try number) <|> boolean <|> string_ <|> identifier)

-- List
list :: Parser Expr
list = do char '(' >> spaces
          lst <- expr `sepBy` spaces 
          spaces >> char ')'
          return $ if length lst == 0
                      then Null
                      else listToPairs lst

-- Any parser followed by optional space
lexeme parser = do p <- parser
                   spaces
                   return p

-- Parse a complete expression
expr :: Parser Expr
expr = (lexeme $ (try number)
              <|> boolean
              <|> string_
              <|> identifier
              <|> list)

-- Read a scheme expression and print it's representation
readExpr :: String -> ThrowsError Expr
readExpr str = case parse (spaces >> expr) "" str of
                    Left err -> throwError $ ParseError err
                    Right val -> return val

-- Parser test
testParser :: IO ()
testParser = sequence_ $ map (putStrLn . show . readExpr) testExpressions

testExpressions = 
    ["+47",
     "()",
     "\"String with \\\"escapes\\\":\\n\\tgoes\\n\\there\"",
     " ( oddly  spaCed ( expreSSion ) ) ",
     unlines ["((numbers +47 -47 0047)", 
              " (booleans #t #f #T #F)",
              " (strings \"Test\" \"asdf\")",
              " (symbols a b c))"],
     "(parse error #a)"]
