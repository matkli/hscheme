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
          putStrLn $ readExpr (args !! 0)

-- Test the show functionality
testShow :: IO ()
testShow = let e1 = Pair (Number 1) (Pair (Number 2) Null)
               e2 = Pair (String "asdf") (Pair (Bool True) (Bool False))
               e3 = Symbol "atom"
               e4 = Null in
           do putStrLn $ show e1
              putStrLn $ show e2
              putStrLn $ show e3
              putStrLn $ show e4

