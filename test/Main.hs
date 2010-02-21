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
-- lets make these test only run once! 
testsuiteStatic = [
    ( "ArithmTest4.hls", show $ 3 + (2 * 3) )
  , ( "FunTest1.hls"   , show $ 1)
  , ( "FunTest2.hls"   , show $ 2) 
  , ( "FunTest3.hls"   , show $ 2)
  , ( "FunTest4.hls"   , show $ 2)
  , ( "FunTest6.hls"   , "(S (S Z))") -- depend on how we render results
  , ( "ListTest3.hls"  , let list = [5,3,1,8,2]
                           in show $ (reverse (take 2 list) 
                              == drop 3 (reverse list)))
  , ( "PrimeTest1.hls"   , show $ True)
  , ( "OptTest1.hls"   , show $ length (take 3 (repeat 4)))
    ]
-- note that we are working on a :: Integer -> [Integer] -> String
-- (Filename, function, absolute value?)
testsuiteDyn = [
    ( "ArithmTest1.hls", \x _ -> show $ 1 + x , False)
  , ( "ArithmTest2.hls", \x _ -> show $ 1 + 2 * x + 4 * 5, False)
  , ( "ArithmTest3.hls", \x _ -> let twice f = f . f
                           in show $ twice twice (+1) x, False)
  , ( "ArithmTest5.hls", \x _ -> show $ x * (x + 1) `div` 2, False)
  , ( "ListTest1.hls"  , \x _ -> show $ length (replicate (fromInteger x) 4) , True)
  , ( "ListTest2.hls"  , \x _ -> show $ length (take (fromInteger x) (repeat 4)), True)
  , ( "PrimeTest2.hls", \_ xs -> 
                let isprime t n = case t * t >= n of
                        True  -> True
                        False -> if (n `mod` t == 0) 
                                  then False
                                  else isprime (t+1) n
                 in show $ all (isprime 2) xs
              , False)
            ]

-- create our test cases
-- This is slow, so we have to think of ways to make it faster.
interpreter = map toTestStatic testsuiteStatic ++ map toTestDyn testsuiteDyn
  where 
    toTestDyn (file, fun, a) = 
        QCTest { name   = file
             , qc   = forAll arbitrary $ \(x :: Integer) (y :: [Integer]) ->
              let x' = cap 1000 $ abs' a x
                  input = defaultInput { inputInteger  = Just x'
                                       , inputIntegers = Just y
                                       }
                  setting = defaultSettings {input = input}
                  -- can we lift IO in some better way?
                  v = unsafePerformIO $ forceInterpreter setting file 
                                 in v == (fun x' y)
               }
    abs' True  x | x < 0     = -x
    abs' True  x | otherwise = x
    abs' False x = x 
    cap y x | x > y     = y
    cap y x | otherwise = x

    toTestStatic (file, value) = 
        I'make'my'own'test { name = file
             , action   = do 
               let setting = defaultSettings
               v <- forceInterpreter setting file 
               let res = (v == value)
               case res of
                    True  -> putStrLn "Pass"
                    False -> putStrLn "Failed"
               return res
               }
