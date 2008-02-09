-- hscheme: A scheme interpreter written in Haskell
--
-- Copyright (C) 2008 Mats Klingberg

module Main where

-- Standard imports
import System.Environment

-- hscheme imports
import Expr
import Parse
import Eval
import Error

main :: IO ()
main = do args <- getArgs
          let s = args !! 0
          if s == "--test"
             then runTests
             else putStrLn $ showEither $ readExpr (args !! 0) >>= eval

runTests :: IO ()
runTests = 
    do putStrLn "Test show:"
       mapM_ (putStrLn . showEither) testExpr
       putChar '\n'
       putStrLn "Parser tests:"
       mapM_ (putStrLn . showEither) testParser
