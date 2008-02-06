-- hscheme: A scheme interpreter written in Haskell
--
-- Copyright (C) 2008 Mats Klingberg

module Main where

-- Standard imports
import System.Environment

-- hscheme imports
import Expr
import Parse

main :: IO ()
main = do args <- getArgs
          let s = args !! 0
          if s == "--test"
             then runTests
             else putStrLn $ show $ readExpr (args !! 0)

runTests :: IO ()
runTests = 
    do putStrLn "Show tests:"
       testShow
       putChar '\n'
       putStrLn "Parser tests:"
       testParser
