-- hscheme: A scheme interpreter written in Haskell
--
-- Copyright (C) 2008 Mats Klingberg

module Main where

-- Standard imports
import System.Environment

-- hscheme imports
import Expr

main :: IO ()
main = do args <- getArgs
          if length args > 0
             then putStrLn $ args !! 0
             else return ()
