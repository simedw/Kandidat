{-# LANGUAGE GADTs #-}
module Types 
  ( Test(..)
  , runTests -- [Test] -> IO ()
  ) where

import Prelude hiding (catch)

import Data.Ratio
import Test.QuickCheck

import Control.Exception

data Test where
  QCTest 
    { name :: String
    , qc   :: Property
    } :: Test
  PassFail 
    { name  :: String
    , tests :: IO [a]
    , run   :: a -> IO Bool
    } :: Test
  I'make'my'own'test 
    { name   :: String
    , action :: IO Bool
    } :: Test

report :: Int -> Int -> IO ()
report suc total = putStrLn $ "outcome: " 
    ++ show suc ++ " / " ++ show total

runTest :: Test -> IO Bool
runTest test = do
   mapM_ putStrLn 
         [ replicate 50 '-'
         , "running: " ++ name test
         , replicate 50 '-'
         ] 
   res <- case test of
       QCTest name qc -> do
            res <- quickCheckWithResult stdArgs {maxSuccess = 10} qc
            case res of
                Success l -> return True
                f -> print f >> return False
       PassFail name tests run -> do
            tests' <- tests
            res <- mapM run tests'
            let suc   = length $ filter id res
                total = length tests'
            report suc total
            return $ suc == total
       I'make'my'own'test name action -> action
   putStrLn $ ""
   return res


runTests :: [Test] -> IO ()
runTests ts = do
    res <- mapM runTest ts
    putStrLn $ replicate 50 '%'
    report (length (filter id res)) (length ts)
