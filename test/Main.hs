{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Parser.SugarParser
--import Parser.Pretty.Test

import System.Directory
import System.FilePath

import System.IO.Unsafe

import Data.List
import Test.QuickCheck

import Types 
import Stg.Input
import Stg.AST (SValue(..), Atom(..))
import Util



main :: IO ()
main = runTests $ 
    {-[ testParser
    , QCTest { name = "parse . pretty ~= id", qc = prop_Pretty_Parse}
    ] ++-} interpreter
{-
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
-}

class BuildSValue a where
    build :: a -> SValue String

instance BuildSValue Integer where
    build x = SCon "I#" [SAtom . ANum $ x]
instance BuildSValue Double where
    build x = SCon "D#" [SAtom . ADec $ x]
instance BuildSValue Char where
    build x = SCon "C#" [SAtom . AChr $ x]
instance BuildSValue Int where
    build = build . toInteger  

instance BuildSValue Bool where
    build True  = SCon "True"  []
    build False = SCon "False" []


instance BuildSValue a => BuildSValue [a] where
    build []     = SCon "Nil" []
    build (x:xs) = SCon "Cons" [build x, build xs]

(--->) :: BuildSValue a => String -> a -> 
        (String, SValue String)
file ---> fun = (file, build fun)


infix 0 --->
infix 0 -->
infix 0 |->

-- |-> for working with lists
(-->), (|->) :: BuildSValue a => String -> (Integer -> [Integer] -> a) -> 
                (String, Integer -> [Integer] -> SValue String, Bool)
file --> fun = (file, \x y -> build $ fun x y, False)
file |-> fun = (file, \x y -> build $ fun x y, True)

-- a map between testfunctions and our semantic excepted functions
-- lets make these test only run once! 
testsuiteStatic = [
    "ArithmTest4.hls" ---> 3 + (2 * 3 :: Integer)
  , "FunTest1.hls"    ---> (1 :: Integer)
  , "FunTest2.hls"    ---> (2 :: Integer)
  , "FunTest3.hls"    ---> (2 :: Integer)
  , "FunTest4.hls"    ---> (2 :: Integer)
  --, "FunTest6.hls"    ---> "(S (S Z))" -- depend on how we render results
  , "ListTest3.hls"   ---> let list = [5,3,1,8,2]
                            in (reverse (take 2 list) 
                                == drop 3 (reverse list))
  , "PrimeTest1.hls"  ---> True
  --, "OptTest1.hls"    ---> length (take 3 (repeat 4))
  --, "OptWithTest1.hls" ---> map (\x -> x*x) [1,2,3,4 :: Integer]
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
  , "LetRecTest1.hls" |-> \x _ -> even x
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
                  setting = LSettings { input   = input
                                      , prelude = "Prelude.hls"
                                      , disableOptimise = False
                                      }
                  -- can we lift IO in some better way?
                  v = unsafePerformIO (loadFile setting file)
                in case forceInterpreter v of 
                    Left err  -> error $ err 
                    Right res -> res == fun x' y
               }
    abs' True  x | x < 0 = -x
    abs' True  x         = x
    abs' False x = x 
    cap y x | x > y     = y
    cap y x | otherwise = x
    
    -- no point in runing static test twice
    toTestStatic (file, value) = 
        I'make'my'own'test { name = file
             , action   = do 
               let setting = LSettings { input = defaultInput
                                       , prelude = "Prelude.hls"
                                       , disableOptimise = False
                                       }
               v <- loadFile setting file 
               case forceInterpreter v of 
                    Left err -> error $ err 
                    Right res -> do 
                     case res == value of
                        True  -> putStrLn "Pass"
                        False -> putStrLn $ "Failed "
                                     ++ "\nExpected: " ++ show value
                                     ++ "\nGot: "      ++ show res 
                     return (res == value)
               }
