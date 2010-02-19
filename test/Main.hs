{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Parser.SugarParser
import Parser.Pretty.Test

import System.Directory
import System.FilePath

import System.IO.Unsafe

import Data.List
import Test.QuickCheck

import Types 
import Interpreter hiding (main)
import Stg.Input

main :: IO ()
main = runTests $ 
    [ testParser
    , QCTest { name = "parse . pretty ~= id", qc = prop_Pretty_Parse}
    ] ++ interpreter

testParser :: Test
testParser = PassFail
    { name = "Parser Sugar"
    , tests = do
        dir <- getCurrentDirectory
        let filepath = dir </> "testsuite"
        files <- getDirectoryContents filepath
        return $ map (\f -> filepath ++ "/" ++ f) 
               $ filter (".hls" `isSuffixOf`) 
               $ sort files
    , run = \file -> do
        str <- readFile file
        case parseSugar str of
            Right _ -> return True
            Left r  -> do putStrLn $ file ++ " fail: " ++ show r 
                          return False
    }

-- a map between testfunctions and our semantic excepted functions
-- note that we are working on a :: Integer -> [Integer] -> String
testsuite = [ ( "ArithmTest1.hls", \_ _ -> show $ 1 + 2)
            , ( "PrimeTest2.hls", \_ xs -> 
                let isprime t n = case t * t >= n of
                        True  -> True
                        False -> if (n `mod` t == 0) 
                                  then False
                                  else isprime (t+1) n
                 in show $ all (isprime 2) xs
               )
            ]

-- create our test cases
interpreter = map toTest testsuite
  where 
    toTest (file, fun) = 
        QCTest { name   = file
             , qc   = forAll arbitrary $ \(x :: Integer) (y :: [Integer]) -> 
              let input = defaultInput { inputInteger  = Just x
                                       , inputIntegers = Just y
                                       }
                  setting = defaultSettings {input = input}
                  -- can we lift IO in some better way?
                  v = unsafePerformIO $ forceInterpreter setting file 
                                 in not (null y) ==> v == (fun x y)
               }

