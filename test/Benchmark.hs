{-# LANGUAGE FlexibleInstances #-}
module Main where

import Criterion
import Criterion.Main
import Control.DeepSeq

import System.Directory
import System.FilePath
import System.Exit

--import Debugger hiding (main)
import Stg.Interpreter
import Stg.PrePrelude
import Stg.Input
import Stg.AST
import Stg.Types
import Stg.Rules
import Stg.Substitution
import Parser.SugarParser
import qualified Parser.Diabetes as D


data Settings = Settings
  { prelude      :: String
  , input        :: Input
  , toGC         :: Bool
  }

defaultSettings = Settings {
    prelude      = "Prelude.hls"
  , input        = defaultInput
  , toGC         = True
  }


{-
 - First of we want to define what files we are going to benchmark
 - and set of data to test with, this will not be random
 -}

benchmarklist :: [(String, [(Integer,[Integer])])]
benchmarklist = 
    [ "Shapes.hls"   --> [(0,[1])]
    , "OptTest7.hls" --> [(5,list)] 
{-    , "OptTest1.hls" --> [(0,[])]
    , "OptTest2.hls" -->  [(0,list)]
    , "OptTest3.hls" -->  [(0,list)]
  --  , "OptTest4.hls" --> [(0,list)]   -- matrix 4 x 4
    ,"OptTest6.hls" -->   [(0,list)]
    , "RSA.hls"      -->  [(0,list)] -}
    ]
 where
   list  = [1..20]
   (-->) = (,)

{- We load and parse all test before invoking the actual benchmark.
 - This is mainly to avoid error from hdd reading time and of course
 - parsing time
 -}

main :: IO ()
main = do
    putStrLn "Loading and parsing benchmarks"
    dir     <- getCurrentDirectory
    prelude <- readFile (dir </> "prelude" </> prelude defaultSettings)
    indata  <- readData dir prelude benchmarklist
    putStrLn "Starting benchmarks"
    defaultMain $ 
        [ bgroup name  [bench 
            (show (i,length li) ++ "[Optimise: " ++ show optimise++"]") $ 
            nf (force (settingsWith input optimise))  (opt optimise code)
            | input@(i,li) <- allinput , optimise <- [True, False] ] 
          | (name,allinput,code) <- indata]
  
  where
    readData _ _ [] = return []
    readData dir prelude ((name,indata):xs) = do
        res  <- readFile (dir </>  "testsuite" </> name)
        case parseSugar (res ++ "\n" ++ prelude) of
           Right fs -> do rest   <- readData dir prelude xs
                          return $ (name, indata, D.run prePrelude fs) : rest
           Left r   -> do putStrLn $ "Failed to parse: " ++ name 
                                   ++ "\nReason: " ++ show r
                          exitFailure
  
    force :: Settings -> [Function String] -> String
    force settings indata = runForce (input settings)  indata

    settingsWith (int,intlist) opt = defaultSettings { input = Input (Just int) (Just intlist) }
    
    opt False x = map removeOPT x
    opt True  x = x

-- NFData has a function rnf, that fully evalues all arguments
instance NFData t => NFData (Function t) where

