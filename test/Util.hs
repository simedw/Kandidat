{-# LANGUAGE PackageImports #-}
module Util where

import "mtl" Control.Monad.State
import Data.Function
import Data.List
import qualified Data.Map as M
import System( getArgs )
import System.Directory
import System.FilePath
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Parser.Diabetes
import qualified Parser.Locals as Locals
import Parser.Pretty.Pretty
import Parser.SugarParser
import Stg.AST
import Stg.Input
import Stg.Interpreter
import Stg.Types
import Stg.Substitution
import Stg.PrePrelude
import qualified Stg.Types as ST

import Stg.Heap (Heap,Location(..))
import qualified Stg.Heap as H


data LoadSettings = LSettings
  { prelude         :: String
  , input           :: Input
  , disableOptimise :: Bool
  }


loadFile :: LoadSettings -> FilePath -> IO [Function String]
loadFile settings file = do
    dir     <- getCurrentDirectory
    prelude <- readFile (dir </>  "prelude" </> prelude settings)
    res     <- readFile (dir </>  "testsuite" </> file)
    case parseSugar (res ++ "\n" ++ prelude) of
        Right fs -> let code = (Locals.localise $ run (createGetFuns (input settings)
                              ++ prePrelude) fs)
                        in  case disableOptimise settings of
                                    True  -> return $ map removeOPT code
                                    False -> return $ code
        Left  r  -> putStrLn ("fail: " ++ show r) >> return []

-- note that PrintCt must be the first thing on the stack
forceInterpreter :: [Function String] -> Either String (SValue String)
forceInterpreter fs = do
    let res = eval fs
        lc  = code . snd . last $ res
    case lc of
        ESVal x -> Right x
        x       -> Left ("fail: Didn't end with ESVal ended with:" ++ show (prExpr PP.text x))

