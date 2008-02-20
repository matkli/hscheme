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
import Parse
import Eval
import Primitives


main :: IO ()
main = do args <- getArgs
          case args of
               [] -> getTopEnv >>= (sequence_ . repeat . readEvalPrint)
               ("--test":_) -> runTests
               _ -> runOne $ args !! 0

-- Evaluate a string
evalString :: [Env] -> String -> IO ()
evalString env expr = 
    do evaled <- runErrorT $ (liftThrows $ readExprs expr) >>= mapM (eval env)
       case evaled of
            Left err -> putStrLn $ show err
            Right [] -> return ()
            Right vals -> mapM_ (putStrLn . show) vals

-- runOne
runOne :: String -> IO ()
runOne expr = getTopEnv >>= flip evalString expr

-- Run simple read-eval-print loop
readEvalPrint :: [Env] -> IO ()
readEvalPrint env = do putStr prompt >> hFlush stdout
                       ln <- getLine
                       evalString env ln

-- prompt
prompt :: String
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
