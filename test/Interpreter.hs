{-# LANGUAGE NamedFieldPuns #-}
module Test.Interpreter where

import Parser.SugarParser
import Parser.Diabetes
import Stg.Interpreter
import System.Directory
import System.FilePath
import Stg.Rules
import Stg.Types
import Text.PrettyPrint

import Parser.Pretty.Pretty
import PrePrelude.PrePrelude

import qualified Data.Map as M

import System( getArgs )
import System.Console.GetOpt

data Settings = Settings
  { steping :: Bool
  , prelude :: String
  , showStgState :: (StgState String -> String)
  , showStgRule  :: (Rule -> String)
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
  }

noheapSettings = defaultSettings { showStgState = stgState True True False }

testInterpreter :: Settings -> FilePath -> IO ()
testInterpreter settings file = do
    dir     <- getCurrentDirectory
    prelude <- readFile (dir </> ".." </> "prelude" </> prelude settings)
    res     <- readFile (dir </> ".." </> "testsuite" </> file)
    case parseSugar (prelude ++ res) of
      Right fs -> mapM_ (\(r, s) -> putStrLn "<-------------->" 
                        >> steps (steping settings) 
                        >> putStrLn (showStgRule settings r 
                        ++ "\n" 
                        ++ showStgState settings s)) $ eval (prePrelude ++ map run fs)

      Left  r  -> do putStr $ "fail: " ++ show r
  where
    steps True  = getChar >> return ()
    steps False = return ()


main :: IO ()
main = do
    args <- getArgs
    print $ show args
--    let ( flags, nonOpts, msgs ) = getOpt RequireOrder options args
    case getOpt RequireOrder options args of
        (flags, [],      [])     -> do
             opts <- foldl (>>=) (return defaultSettings) flags
             testInterpreter opts "FunTest1.hls"
        (_,     nonOpts, [])     -> error $ "unrecognized arguments: " ++ unwords nonOpts
        (_,     _,       msgs)   -> error $ concat msgs ++ usageInfo header options

data Flag = Version

options :: [OptDescr (Settings -> IO Settings)] 
options = [ Option ['S'] ["step"] (ReqArg setSteping "BOOL") "step through" ]

setSteping :: String -> Settings -> IO Settings
setSteping arg s = return $ s { steping = read arg}

header = "Usage: main [OPTION...]"
