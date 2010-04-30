{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Parser.SugarParser

import Control.Applicative

import System.Directory
import System.FilePath

import System.IO.Unsafe

import Data.List
import Test.QuickCheck

import Types 
import Stg.Input
import Stg.AST (SValue(..), Atom(..))
import Util

{-
 BuildSValue are used to convert haskell datatypes to our datatypes
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

instance BuildSValue t => BuildSValue (Construct t) where
    build (Construct name vals) = SCon name (map build vals)

instance BuildSValue Bool where
    build True  = SCon "True"  []
    build False = SCon "False" []

instance BuildSValue a => BuildSValue [a] where
    build []     = SCon "Nil" []
    build (x:xs) = SCon "Cons" [build x, build xs]

data Construct t = Construct String [t]

cons = Construct

-- like maybe but for Tests
test f g (StaticTest v)  = f v
test f g (DynamicTest v) = g v

data Tests
    = StaticTest (SValue String) 
    | DynamicTest (Indata -> SValue String)

data Indata = I
    { int  :: Integer
    , ints :: [Integer]
    , dbl  :: Double
    , dbls :: [Double]
    , chr  :: Char
    , str  :: String
    }
  deriving Show

instance Arbitrary Indata where
    arbitrary = I <$> arbitrary 
                  <*> arbitrary 
                  <*> arbitrary 
                  <*> arbitrary
                  <*> arbitrary
                  <*> arbitrary
                  

-- For static tests
(--->) :: BuildSValue s => FilePath -> s -> (FilePath, Tests)
file ---> v = (file, StaticTest $ build v)

-- For dynamic (using input) tests
(-->) :: BuildSValue s => FilePath -> (Indata -> s) -> (FilePath, Tests)
file --> f = (file, DynamicTest $ \r -> build $ f r)

infix 0 --->
infix 0 -->

interpreter = map (\(file, ts) -> test testStatic testDynamic ts file)
  where
    testStatic value file = I'make'my'own'test 
        { name   = file
        , action = do 
               let setting = LSettings { input = defaultInput
                                       , prelude = "Prelude.hls"
                                       , disableOptimise = False
                                       }
               v <- loadFile setting file 
               case forceInterpreter v of 
                    Left err -> putStrLn ("Failed: " ++ err) >> return False
                    Right res -> do 
                     case res == value of
                        True  -> putStrLn "Pass"
                        False -> putStrLn $ "Failed "
                                     ++ "\nExpected: " ++ show value
                                     ++ "\nGot: "      ++ show res 
                     return (res == value)
        } 
    testDynamic f file = QCTest
        { name = file
        , qc   = forAll arbitrary $ \indata ->
            let input   = defaultInput { inputInteger  = Just (int  indata)
                                       , inputIntegers = Just (ints indata)
                                       , inputDouble   = Just (dbl  indata)
                                       , inputDoubles  = Just (dbls indata)
                                       , inputString   = Just (str  indata)
                                       }

                setting = LSettings { input   = input
                                    , prelude = "Prelude.hls"
                                    , disableOptimise = False
                                    }
                v       = unsafePerformIO (loadFile setting file)
             in case forceInterpreter v of
                Left err  -> False
                Right res -> res == f indata
        }

main :: IO ()
main = runTests $ interpreter testsuite

{-
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
    ] ++ optTests
    
optTests
  = [
      "OC00function.hls" ---> (5 :: Integer)
    , "OC01function.hls" ---> (5 :: Integer)
    , "OC02function.hls" ---> (5 :: Integer)
    , "OC03functionPAP.hls" ---> (5 :: Integer)
    , "OC04functionPAP.hls" ---> (5 :: Integer)
    , "OC05functionPAP.hls" ---> (5 :: Integer)
    , "OC06functionPAP.hls" ---> (5 :: Integer)
    , "OC07functionPAP.hls" ---> (5 :: Integer)
    , "OC08functionPAP.hls" ---> (5 :: Integer)
    , "OC09functionUnknownCall.hls" ---> (5 :: Integer)
    , "OC10functionCtoApp.hls.hls" ---> (5 :: Integer)
    , "OC11functionCtoApp.hls.hls" ---> (5 :: Integer)
    , "OC12caseselection.hls" ---> (5 :: Integer)
    , "OC13caseselection.hls" ---> (5 :: Integer)
    , "OC14caseselection.hls" ---> (5 :: Integer)
    , "OC15caseselection.hls" ---> (5 :: Integer)
    , "OC16caseunrolling.hls" ---> (5 :: Integer)
    , "OC17letincase.hls" ---> (5 :: Integer)
    , "OC18letincase.hls" ---> (5 :: Integer)
    , "OC19letincase.hls" ---> (5 :: Integer)
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


-}



testsuite :: [(FilePath, Tests)]
testsuite = 
  [ "ArithmTest1.hls"         --> \I {..} -> 1 + int
  , "ArithmTest1-double.hls"  --> \I {..} -> 1.0 + dbl
  , "ArithmTest2.hls"         --> \I {..} -> 1 + 2 * int + 4 * 5
  , "ArithmTest3.hls"         --> \I {..} -> int + 4
  , "ArithmTest4.hls"        ---> 3 + 2 * (3 :: Integer)
  , "ArithmTest5.hls"         --> \I {..} -> int * (int + 1 ) `div` 2
  , "ArithmTest6.hls"        ---> sum [2,3,4 :: Integer]
  
  , "FunTest1.hls"    ---> (1 :: Integer)
  , "FunTest2.hls"    ---> (2 :: Integer)
  , "FunTest3.hls"    ---> (2 :: Integer)
  , "FunTest4.hls"    ---> (2 :: Integer)
  
  , "LamLift1.hls"    ---> False
  , "LamTest1.hls"    ---> (6 :: Integer)
  , "LetRecTest1.hls"  --> \I {..} -> even int
  , "ListTest1.hls"    --> \I {..} -> length $ replicate (fromInteger int) 4
  
  , "ListTest1.hls"    --> \I {..} -> length $ replicate (fromInteger int) 4
  , "ListTest2.hls"    --> \I {..} -> length $ take (fromInteger int) $ repeat 4
  , "ListTest3.hls"   ---> let list = [5,3,1,8,2]
                            in (reverse (take 2 list) 
                                == drop 3 (reverse list))
  , "ListTest4.hls"   ---> True
  , "ListTest5.hls"   ---> True 
  , "ListTest6.hls"   ---> sort (reverse (take 3 [(0 :: Integer)..]))
  , "ListTest7.hls"    --> \I {..} -> sort (reverse (take (fromInteger int) [(0 :: Integer),-1..]))

  , "NegTest1.hls"    ---> (0 :: Integer)
  , "PrimeTest1.hls"  ---> True
  , "StringTest1.hls" ---> True
  ] ++ optTests

optTests = [
     "OC00function.hls"     ---> (5 :: Integer)
    , "OC01functionPAP.hls" ---> (5 :: Integer)
    , "OC02function.hls" ---> (5 :: Integer)
    , "OC03function.hls" ---> (5 :: Integer)
    , "OC04function.hls" ---> (5 :: Integer)
    , "OC05function.hls" ---> (5 :: Integer)
    , "OC06function.hls" ---> (5 :: Integer)
    , "OC07function.hls" ---> (5 :: Integer)
    , "OC08function.hls" ---> [2,2,2 :: Integer]
    , "OC09unknownCall.hls" ---> [5,5,5,5 :: Integer]
    , "OC10CtOApp.hls" ---> (5 :: Integer)
    , "OC11CtOApp.hls" ---> (5 :: Integer)
    , "OC12CtOApp.hls" ---> (5 :: Integer)
    , "OC13CtOApp.hls" ---> (5 :: Integer)
    
    
    , "OC14caseselection.hls" --> \I {..} -> map (if int > 0 then (+ 1) 
                                                  else subtract 1) ints
    , "OC15caseselection.hls" --> \I {..} -> map (if int > 0 then (+ 1) 
                                                  else subtract 1) ints
    , "OC16caseselection.hls" --> \I {..} -> map (if int > 0 then (+ 1) 
                                                  else subtract 1) ints
    , "OC17caseselection.hls" --> \I {..} -> map (if int > 0 then (+ 1) 
                                                  else subtract 1) ints
    
    
    , "OC18caseunrolling.hls" ---> (5 :: Integer)
    , "OC19letincase.hls" ---> (5 :: Integer)
    , "OC20letincase.hls" ---> (5 :: Integer)
    , "OC21letincase.hls" ---> (5 :: Integer)
    , "OC22casedPAP.hls" ---> (5 :: Integer)
    
    , "OC23casedPAP.hls" --> \I {..} -> map (if int > 0 then (+ 5) 
                                                  else subtract 5) ints
    
    , "OC24abyssCon.hls" ---> cons "X" [2 :: Integer]
    , "OC25abyssConAlt.hls" ---> cons "X" [2 :: Integer]
    , "OC26abyssConSimon.hls" ---> [4,6 :: Integer]
    , "OC27power.hls"  --> \I{..} -> map (* (abs int)) ints
    , "OC28power.hls"  --> \I{..} -> map (+ (abs int)) ints
    , "OC29filter.hls" --> \I{..} -> filter (/= dbl) dbls
    , "OC30filter.hls" --> \I{..} -> filter (\x -> x > int * 2 && x < int * 3) 
                                             ints
    , "OC31zipWith.hls" --> \I{..} -> zipWith (+) [0..] ints 
    ]
