-- | HScheme: A scheme interpreter written in Haskell.
--
-- Copyright 2008 Mats Klingberg
--
-- This file is part of hscheme and is licensed under the GNU GPL, see the
-- LICENSE file for the full license text.

module Main where

-- Standard imports
import System.Environment
import System.IO
import Control.Monad.Error

-- hscheme imports
import Types
import Parse
import Eval
import Primitives

-- | Main program
main :: IO ()
main = do args <- getArgs
          case args of
               [] -> getTopEnv >>= (sequence_ . repeat . readEvalPrint)
               ("--test":_) -> runTests
               _ -> runOne $ head args

-- | Run simple read-eval-print loop.
readEvalPrint :: [Env] -> IO ()
readEvalPrint env = do putStr prompt >> hFlush stdout
                       ln <- getLine
                       evalString env ln

-- | Evaluate a scheme expression, given as a string, in an environment.
evalString :: [Env] -> String -> IO ()
evalString env expr =
    do evaled <- runErrorT $ liftThrows (readExprs expr) >>= mapM (eval env)
       case evaled of
            Left err -> print err
            Right [] -> return ()
            Right vals -> mapM_ print vals

-- | Evaluate a single expression in a new top environment.
runOne :: String -> IO ()
runOne expr = getTopEnv >>= flip evalString expr

-- | REPL prompt
prompt :: String
prompt = ">>> "

-- | Return a top-level environment.
getTopEnv :: IO [Env]
getTopEnv = fmap (:[]) getPrimitiveEnv

-- | Run module test cases.
runTests :: IO ()
runTests =
    do putStrLn "Test show:"
       mapM_ print testExpr
       putChar '\n'
       putStrLn "Parser tests:"
       mapM_ (putStrLn . showEither) testParser
       putChar '\n'
       putStrLn "Eval tests:"
       env <- getTopEnv
       mapM_ (runErrorT >=> putStrLn . showEither) $ testEval env
