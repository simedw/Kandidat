{-# LANGUAGE GADTs #-}
module Main where

import Parser.SugarParser
import Parser.Pretty.Test

import System.Directory
import System.FilePath

import Test.QuickCheck

import Data.List

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


main :: IO ()
main = return ()

{-
runTests :: [Test] -> IO ()
runTests tests = do
  suc <- runTests' 0 tests
  pline
  putStrLn $ "summary"
  pline
  putStrLn $ "of " ++ show (length tests) ++ ", " ++ show suc ++ " succeded"
  where
    title = "running: "
    pline = putStrLn $ replicate 68 '-'

    runTests' :: Int -> [Test] -> IO Int
    runTests' n [] = return n
    runTests' n (t : ts) = do
      pline
      putStrLn $ title ++ name t 
      pline
      res <- action t
      case res of
        Ok   -> runTests' (n+1) ts
        Fail -> runTests' n ts

main = do 
    testParser 
    runTests [ Test { name = "QuickCheck pretty", action = qcPretty}
             , Test { name = "QuickCheck pretty 2", action = qcPretty}
             ]

testParser :: IO ()
testParser = do
    putStrLn "running: Parser Sugar"
    dir <- getCurrentDirectory
    putStrLn dir
    let filepath = dir </> "testsuite"
    files <- getDirectoryContents filepath
    let files' = map (\f -> filepath ++ "/" ++ f) 
               $ filter (".hls" `isSuffixOf`) 
               $ sort files
    res <- mapM testParse files'
    putStrLn $ "Parser Sugar outcome : " ++ show (length $ filter id res) ++ "/" ++ show (length res)
  where 
    testParse file = do
        str <- readFile file
        case parseSugar str of
            Right _ -> return True
            Left r  -> do putStrLn $ file ++ " fail: " ++ show r 
                          return False


qcPretty :: TIO
qcPretty = do 
  res <- quickCheckResult prop_Pretty_Parse
  case res of
    Success l -> return Ok
    f -> print f >> return Fail

-}
