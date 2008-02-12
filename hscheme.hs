-- hscheme: A scheme interpreter written in Haskell
--
-- Copyright (C) 2008 Mats Klingberg

module Main where

-- Standard imports
import System.Environment
import IO
import Control.Monad.Error

-- hscheme imports
import Types
import Expr
import Parse
import Eval
import Error
import Primitives


main :: IO ()
main = do args <- getArgs
          case args of
               [] -> sequence_ $ repeat $ runRepl
               ("--test":_) -> runTests
               expr -> runOne $ args !! 0

-- evaluate an expression and print the result
evalAndPrint :: [Env] -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

-- Evaluate a string
evalString :: [Env] -> String -> IO String
evalString env expr = liftM showEither $ runErrorT 
    $ (liftThrows $ readExpr expr) >>= eval env

-- runOne
runOne :: String -> IO ()
runOne expr = getTopEnv >>= flip evalAndPrint expr

-- Run simple read-eval-print loop
runRepl :: IO ()
runRepl = do putStr prompt >> hFlush stdout
             ln <- getLine
             runOne ln

-- prompt
prompt = ">>> "

-- Top-level environment
getTopEnv :: IO [Env]
getTopEnv = getPrimitiveEnv >>= return . (:[])

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
       env <- getTopEnv
       mapM_ (runErrorT >=> putStrLn . showEither) $ testEval env
