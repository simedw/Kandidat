module Main where

import Parser.SugarParser
import Parser.Pretty.Test

import System.Directory
import System.FilePath

import Data.List
import Test.QuickCheck

import Types 

main :: IO ()
main = runTests
    [ testParser
    , QCTest { name = "parse . pretty ~= id", qc = prop_Pretty_Parse}
    ]

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
