{-# LANGUAGE NamedFieldPuns #-}
module Interpreter where

import Parser.SugarParser
import Parser.Diabetes
import Stg.Interpreter
import System.Directory
import System.FilePath
import Stg.AST
import Stg.Rules
import Stg.Types
import Stg.Input
import Text.PrettyPrint

import Parser.Pretty.Pretty
import PrePrelude.PrePrelude

import qualified Data.Map as M

import Data.List
import Data.Function

import System( getArgs )
import System.Console.GetOpt

data Settings = Settings
  { steping :: Bool
  , prelude :: String
  , showStgState :: (StgState String -> String)
  , showStgRule  :: (Rule -> String)
  , input :: Input
  , quiet :: Bool
  }

stgState :: Bool -> Bool -> Bool -> StgState String -> String
stgState sc ss sh st@(StgState {code, stack, heap}) = 
       showi sc "code" (show (prExpr (text . show) code))
    ++ showi ss ("stack(" ++ show (length stack) ++ ")") (show stack) 
    ++ showi sh ("heap("++ show (M.size heap) ++")") (concat [ show (id, prObj (text . show) obj) ++ "\n\t"
                             | (id, obj) <- M.toList heap])
  where
    showi :: Bool -> String -> String -> String
    showi True s d  = s ++ ": " ++ d ++ "\n"
    showi False s _ = s ++ "\n"



defaultSettings = Settings {
    steping = True
  , prelude = "Prelude.hls"
  , showStgState = stgState True True True
  , showStgRule = show 
  , input = defaultInput
  , quiet = False
  }

noheapSettings = defaultSettings { showStgState = stgState True True False }


-- forceInterpreter forces the return value to be evaluted
-- so we can display it in a nice fashion
-- current directory is the base directory to run `cabal test'
forceInterpreter :: Settings -> FilePath -> IO String
forceInterpreter settings file = do
    dir     <- getCurrentDirectory
    prelude <- readFile (dir </> "prelude" </> prelude settings)
    res     <- readFile (dir </> "testsuite" </> file)
    case parseSugar (prelude ++ res) of
      Right fs -> let res = runForce (input settings) (prePrelude ++ run fs)
                   in return res
      Left  r  -> do return $ "fail: " ++ show r



testInterpreter :: Settings -> FilePath -> IO ()
testInterpreter settings file = do
    dir     <- getCurrentDirectory
    prelude <- readFile (dir </> ".." </> "prelude" </> prelude settings)
    res     <- readFile (dir </> ".." </> "testsuite" </> file)
    -- prelude must be last, otherwise parse error messages get wrong line numbers!
    case parseSugar (res ++ prelude) of  
      Right fs -> do
        let trace = eval (input settings) (prePrelude ++ run fs)
        mapM_ (\(r, s) -> putStrLn "<-------------->" 
                        >> steps (steping settings) 
                        >> putStrLn (showStgRule settings r 
                        ++ "\n" 
                        ++ showStgState settings s)) trace
        print $ map (\ list -> (head list, length list))
              $ group $ sort $ map fst  trace
      Left  r  -> do putStrLn $ "fail: " ++ show r
  where
    steps True  = getChar >> return ()
    steps False = return ()

main :: IO ()
main = do
    -- First argument is the filename
    file:args <- getArgs
    print $ args
--    let ( flags, nonOpts, msgs ) = getOpt RequireOrder options args
    case getOpt RequireOrder options args of
        (flags, [],      [])     -> do
             opts <- foldl (>>=) (return defaultSettings) flags
             testInterpreter opts file
        (_,     nonOpts, [])     -> error $ "unrecognized arguments: " ++ unwords nonOpts
        (_,     _,       msgs)   -> error $ concat msgs ++ usageInfo header options

data Flag = Version

options :: [OptDescr (Settings -> IO Settings)] 
options = 
    [ Option ['S'] ["step"] (ReqArg setSteping "BOOL") "step through"
    , Option ['V'] ["visible"] (ReqArg setVisible "BOOL BOOL BOOL") "show code stack heap"
    , Option ['I'] ["integerinput"] 
        (ReqArg setInputInteger "Integer") 
        "single integer input"
    , Option ['L'] ["listinput"] 
        (ReqArg setInputIntegers "[Integer]") 
        "integer list input"
    ]


setSteping :: String -> Settings -> IO Settings
setSteping arg s = return $ s { steping = read arg }

setVisible :: String-> Settings -> IO Settings
setVisible ind set = let (arg: arg2: arg3:_) = lines ind
                         h = read arg3
                         s = read arg2
                         c = read arg
                     in return $ set {  showStgState =  stgState c s h }



setInputInteger :: String -> Settings -> IO Settings
setInputInteger arg s = 
    return $ s { input = (input s) { inputInteger = Just (read arg) }}

setInputIntegers :: String -> Settings -> IO Settings
setInputIntegers arg s = 
    return $ s { input = (input s) { inputIntegers = Just (read arg) }}


header = "Usage: main [OPTION...]"
