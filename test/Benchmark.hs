{-# LANGUAGE FlexibleInstances #-}
module Main where

import Criterion
import Criterion.Main
import Control.DeepSeq

import System.Directory
import System.FilePath
import System.Exit

--import Debugger hiding (main)
import qualified Parser.Locals as Locals
import Stg.Interpreter
import Stg.PrePrelude
import Stg.Input
import Stg.AST
import Stg.Types
import Stg.Rules
import Stg.Substitution
import Parser.SugarParser
import qualified Parser.Diabetes as D

import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Parser.Pretty.Pretty
import Util

{-
 - First of we want to define what files we are going to benchmark
 - and set of data to test with, this will not be random
 -}

benchmarklist :: [(String, (Integer,[Integer]))]
benchmarklist = 
   [ --"Shapes.hls"   --> [(0,[1])]
     "OptTest7.hls" -->  (100,list)
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
    files <- loadFiles benchmarklist "Prelude.hls"
    
    putStrLn "Starting benchmarks"
    defaultMain $
        [ bgroup name [bench
                title
                (nf force optcode)
                | (optcode, title) <- code
                ] 
        | (name, code) <- files]
                            

  where
    loadFiles [] _ = return []
    loadFiles ((name, (i,l)):xs) prelude = do
        files <- sequence [ do 
            f <- loadFile (LSettings prelude (Input (Just i) (Just l)) opt) name
            return (f, "Input: " ++ show (i,length l) ++ " Optimise: " ++ show (not opt))  
                          | opt <- [True, False]]
        rest <- loadFiles xs prelude
        return $ (name, files) : rest 
    
    force code = case forceInterpreter code of
        Left err -> error $ "Benchmark: " ++ err
        Right tree -> tree



-- | We should evaluate the arguments as far was we can
instance NFData t => NFData (SValue t) where
    rnf v = case v of
        SAtom at    -> rnf at
        SCon _ values -> rnf values 
        SFun -> ()

instance NFData t => NFData (Atom t) where
    rnf x = case x of
        AVar _   -> error "NFData on Var"
        ANum v   -> rnf v
        ADec v   -> rnf v
        AChr v   -> rnf v
