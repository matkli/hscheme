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

