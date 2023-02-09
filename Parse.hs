-- | Scheme parser module.
--
-- Copyright 2008, 2023 Mats Gan Klingberg
--
-- This file is part of hscheme and is licensed under the GNU GPL, see the
-- LICENSE file for the full license text.

module Parse (
    readExpr,
    readExprs,
    testParser
  ) where

-- Standard imports
import Text.ParserCombinators.Parsec
import Control.Monad.Error
import Data.Char

-- Local imports
import Types

-- | Numerical literals
--
-- Only integers so far.
number :: Parser Expr
number = do sign <- option '+' $ oneOf "+-"
            rest <- many1 digit
            return $ Number $ if sign == '+' 
                                 -- read doesn't like leading '+'
                                 then read rest
                                 else read (sign:rest)

-- | Boolean literals
boolean :: Parser Expr
boolean = do c <- char '#' >> oneOf "tTfF"
             return $ case toLower c of
                           'f' -> Bool False
                           't' -> Bool True

-- | Identifiers
--
-- This is not quite R5RS compliant.
identifier :: Parser Expr
identifier = 
    let initial = letter <|> oneOf "!$%&*+-/:<=>?@^_~"
    in do first <- initial
          rest <- many $ initial <|> digit
          return $ Symbol $ map toLower $ first:rest

-- | Strings
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

-- | Quoted expressions
quoted :: Parser Expr
quoted = do lexeme $ char '\''
            e <- expr
            return $ List [Symbol "quote", e]

-- | Lists
--
-- This handles both proper lists and dotted lists (pairs).
list :: Parser Expr
list = do lexeme $ char '('
          lst <- many expr
          proper lst <|> dotted lst
    where proper :: [Expr] -> Parser Expr
          proper lst = char ')' >> return (List lst)
          dotted lst = do lexeme $ char '.'
                          cdr <- expr
                          char ')'
                          return $ Dotted lst cdr

-- | Skip the result of a single parser.
skip :: CharParser st a -> CharParser st ()
skip = (>> return ())

-- | Comments
comment :: CharParser st ()
comment = do char ';'
             skipMany $ noneOf "\n"
             skip (char '\n') <|> eof
          <?> "comment"

-- | Whitespace (or comment)
whiteSpace :: CharParser st ()
whiteSpace = skipMany $ skip space <|> comment

-- | Any parser followed by optional space.
lexeme ::  GenParser Char st t -> GenParser Char st t
lexeme parser = do p <- parser
                   whiteSpace
                   return p

-- | Parse a complete expression.
expr :: Parser Expr
expr = lexeme (try number
               <|> boolean
               <|> string_
               <|> identifier
               <|> quoted
               <|> list) 
              <?> "expression"

-- | Read (possibly multiple) scheme expressions
readExprs :: String -> ThrowsError [Expr]
readExprs str = case parse (whiteSpace >> many (whiteSpace >> expr)) "" str of
                    Left err -> throwError $ ParseError err
                    Right val -> return val

-- | Read a /single/ scheme expression.
readExpr :: String -> ThrowsError Expr
readExpr = readExprs >=> checkSingle
    where checkSingle :: [Expr] -> ThrowsError Expr
          checkSingle [e] = return e
          checkSingle exprs = throwError $ Default "A single expression was expected"

-- | Test parser
testParser :: [ThrowsError Expr]
testParser = map readExpr testExpressions

-- | Parser test expressions
testExpressions :: [String]
testExpressions = [
    "+47",
    "()",
    "\"String with \\\"escapes\\\":\\n\\tgoes\\n\\there\"",
    " ( oddly  spaCed ( expreSSion ) ) ",
    "(this is a dotted . list)",
    unlines ["((numbers +47 -47 0047)", 
             " (booleans #t #f #T #F)",
             " (strings \"Test\" \"asdf\")",
             " (symbols a b c))"],
    "(parse error #a)"
  ]
