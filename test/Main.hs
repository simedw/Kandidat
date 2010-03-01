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
import qualified Stg.PrePrelude as PP

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


-- Show the results from the test results like the results from the programs
class Show a => ShowResult a where
  showResult :: a -> String
  showResult = show

instance ShowResult Integer where
  showResult n = "(" ++ PP.numCon ++ " " ++ show n ++ ")"
instance ShowResult Int where
  showResult n = "(" ++ PP.numCon ++ " " ++ show n ++ ")"
instance ShowResult Double where
  showResult d = "(" ++ PP.decCon ++ " " ++ show d ++ ")"
instance ShowResult Bool where
instance ShowResult a => ShowResult [a] where
  showResult []     = "Nil"
  showResult (x:xs) = "(" ++ "Cons " ++ showResult x ++ " " ++ showResult xs ++ ")"

(--->) :: ShowResult a => String -> a -> 
        (String, String)
file ---> fun = (file, showResult fun)

infix 0 --->
infix 0 -->
infix 0 |->

-- |-> for working with lists
(-->), (|->) :: ShowResult a => String -> (Integer -> [Integer] -> a) -> 
                (String, Integer -> [Integer] -> String, Bool)
file --> fun = (file, \x y -> showResult $ fun x y, False)
file |-> fun = (file, \x y -> showResult $ fun x y, True)

-- a map between testfunctions and our semantic excepted functions
-- lets make these test only run once! 
testsuiteStatic = [
    "ArithmTest4.hls" ---> 3 + (2 * 3 :: Integer)
  , "FunTest1.hls"    ---> (1 :: Integer)
  , "FunTest2.hls"    ---> (2 :: Integer)
  , "FunTest3.hls"    ---> (2 :: Integer)
  , "FunTest4.hls"    ---> (2 :: Integer)
--  , "FunTest6.hls"    ---> "(S (S Z))" -- depend on how we render results
  , "ListTest3.hls"   ---> let list = [5,3,1,8,2]
                            in (reverse (take 2 list) 
                                == drop 3 (reverse list))
  , "PrimeTest1.hls"  ---> True
  , "OptTest1.hls"    ---> length (take 3 (repeat 4))
  , "ListTest6.hls"   ---> sort (reverse (take 3 [(0 :: Integer)..]))
  , "NegTest1.hls"    ---> (-1) - (-1 :: Integer)
  , "StringTest1.hls" ---> True
    ]
-- note that we are working on a :: Integer -> [Integer] -> String
-- (Filename, function, absolute value?)
testsuiteDyn =
  [ "ArithmTest1.hls" --> \x _ -> 1 + x
  , "ArithmTest2.hls" --> \x _ -> 1 + 2 * x + 4 * 5
  , "ArithmTest3.hls" --> \x _ -> let twice f = f . f
                                   in twice twice (+1) x
  , "ArithmTest5.hls" --> \x _ -> x * (x + 1) `div` 2
  , "ListTest1.hls"   |-> \x _ -> length (replicate (fromInteger x) 4)
  , "ListTest2.hls"   |-> \x _ -> length (take (fromInteger x) (repeat 4))
  , "PrimeTest2.hls"  --> \_ xs -> 
              let isprime t n = case t * t > n of
                      True  -> True
                      False -> if (n `mod` t == 0) 
                                then False
                                else isprime (t+1) n
               in all (isprime 2) xs
  , "ListTest7.hls"   |-> \x _ -> sort [0,-1.. -x+1] -- A little slow :)
          
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
                                 in v == fun x' y
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
               let res = v == value
               case res of
                    True  -> putStrLn "Pass"
                    False -> putStrLn $ "Failed "
                                     ++ "\nExpected: " ++ show value
                                     ++ "\nGot: "      ++ show v 
               return res
               }
