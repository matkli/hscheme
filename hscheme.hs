-- hscheme: A scheme interpreter written in Haskell
--
-- Copyright (C) 2008 Mats Klingberg

module Main where

-- Standard imports
import System.Environment
import IO

-- hscheme imports
import Expr
import Parse
import Eval
import Error

main :: IO ()
main = do args <- getArgs
          case args of
               [] -> sequence_ $ repeat $ runRepl
               ("--test":_) -> runTests
               expr -> putStrLn $ showEither $ readExpr (args !! 0) >>= eval

-- Run simple read-eval-print loop
runRepl :: IO ()
runRepl = do putStr prompt >> hFlush stdout
             ln <- getLine
             putStrLn $ showEither $ readExpr ln >>= eval

-- prompt
prompt = ">>> "


-- Run module test cases
runTests :: IO ()
runTests = 
    do putStrLn "Test show:"
       mapM_ (putStrLn . show) testExpr
       putChar '\n'
       putStrLn "Parser tests:"
       mapM_ (putStrLn . showEither) testParser
       putChar '\n'
       putStrLn "Eval tests:"
       mapM_ (putStrLn . showEither) testEval
