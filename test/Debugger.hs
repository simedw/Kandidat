{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
module Debugger where

import "mtl" Control.Monad.State
import Data.Function
import Data.List
import qualified Data.Map as M
import System( getArgs )
import System.Console.GetOpt
import System.Directory
import System.FilePath
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import System.Console.Haskeline (InputT, outputStrLn, getInputLine) 
import qualified System.Console.Haskeline as Hl

import Text.ParserCombinators.Parsec
import Parser.Diabetes
import qualified Parser.Locals as Locals
import Parser.Pretty.Pretty
import Parser.SugarParser
import Parser.STGParser
import Stg.AST
import Stg.GC
import Stg.Input
import Stg.Interpreter
import Shared.BoxPrimitives
import Stg.Rules
import Stg.Types hiding (settings)
import qualified Stg.Types as ST

import Stg.Heap (Heap,Location(..))
import qualified Stg.Heap as H

import Util

data Settings = Settings
  { steping      :: Bool
  , lset         :: LoadSettings
  , quiet        :: Bool
  , forceStg     :: Bool
  , toGC         :: Bool
  }

type Result = (Rule, StgState String)

data BreakPoint
    = AtRule Rule
    | AtCall String
    deriving (Eq, Show)

evalBP :: BreakPoint -> Result -> Bool
evalBP bp (rule, _) = case bp of
    AtRule r -> r == rule
    --AtCall id -> case code of 
        --ECall fid _ -> id == fid

{-
stgState :: Bool -> Bool -> Bool -> StgState String -> String
stgState sc ss sh st@(StgState {code, stack, heap}) = 
       showi sc "code" (show (prExpr PP.text code))
    ++ showi ss ("stack(" ++ show (length stack) ++ ")") (show stack) 
    ++ showi sh ("heap("++ show (H.size heap) ++")") (concat [ show (id, prObj PP.text obj) ++ "\n\t"
                             | (id, obj) <- H.toList heap])
  where
    showi :: Bool -> String -> String -> String
    showi True s d  = s ++ ": " ++ d ++ "\n"
    showi False s _ = s ++ "\n"

-}

defaultSettings = Settings {
    steping      = True
  , lset         = LSettings "Prelude.hls" defaultInput False
  , quiet        = False
  , forceStg     = False
  , toGC         = True
  }



--noheapSettings = defaultSettings { showStgState = stgState True True False }



data InterpreterState = IS
    { settings :: Settings
    , stgm     :: StgMState String
    , history  :: [Result]
    , breakPoints  :: [BreakPoint]
    }


testInterpreter :: Settings -> FilePath -> IO ()
testInterpreter set file = do
    fs <- loadFile (lset set) file 
    let st = initialState fs
    evalStateT (Hl.runInputT Hl.defaultSettings (loop st)) IS
            { settings = set
            , stgm     = initialStgMState
            , history  = []
            , breakPoints = []
            } 


loop :: StgState String -> InputT (StateT InterpreterState IO) ()
loop originalState  = do
    minput <- getInputLine "% " 
    set <- lift $ gets settings
    let st = (if toGC set then gc else id) originalState
    case minput of
        Nothing -> return ()
        Just xs -> case words xs of
            [] -> evalStep 1 st
            [":q"]   -> return ()
            ":v" : xs    -> loopView st xs
            ":view" : xs -> loopView st xs
            ":bp"  : xs -> addbp xs >> loop st
            [":bpo"] -> addbp ["rule", "ROptimise"] >> evalStep 1000000000 st
            [":bpd"] -> addbp ["rule", "ROpt", "ORDone"] >> evalStep 100000000 st
            [":back", num] -> case reads num of
                ((x, "") : _) -> evalStep (negate x) st
                _ -> loop st
            [":step", num] -> case reads num of
                ((x, "") : _) -> evalStep x st
                _ -> loop st
  --          [":force!"] -> forcing st >> loop st
  --          [":force", var] -> forceit st var >> loop st
            [":h"] -> printHelp >> loop st
            [":help"] -> printHelp >> loop st
            input -> do
                outputStrLn $ "oh hoi: " ++ unwords input
                loop st
  where
    gc = mkGC ["$True", "$False"]

    bp :: [BreakPoint] -> Result -> Maybe BreakPoint
    bp [] _ = Nothing
    bp (b : bs) r | evalBP b r = Just b
                  | otherwise  = bp bs r
    {-
    forcing st = do
        stg <- lift $ gets stgm
        outputStrLn . fst $ runState (force st) stg
    
    forceit st var = do
        stg <- lift $ gets stgm
        case runParser Parser.STGParser.atom () "" var of
            Left r  -> return ()
            Right x -> do
                s <- liftIO $ catch (return $ Just . fst $ runState (force $ st {code = (EAtom x)} ) stg) (\_ -> return $ Nothing)
                case s of
                    Nothing -> outputStrLn "Something bad has happend"
                    Just x  -> outputStrLn x
    -}
    evalStep n s | n == 0 = printSummary s
                 | n < 0  = do
        hist <- lift . gets $ history
        case hist of
            [] -> printSummary s
            (_, s') : xs -> do
                lift . modify $ \set -> set { history = xs }
                evalStep (n + 1) s'
                 | otherwise = do
        stg <- lift $ gets stgm
        bps <- lift . gets $ breakPoints
        case runStgM (step s) stg of
            (Nothing, stg') -> do
                outputStrLn "No Rule applied"
                loop s
            (Just res@(_, s'), stg') | Just b <- bp bps res -> do
                addHistory res
                lift . modify $ \set -> set { stgm = stg' }
                outputStrLn $ "BreakPoint! " ++ show b
                printSummary s'
                                     | otherwise -> do
                addHistory res
                lift . modify $ \set -> set { stgm = stg' }
                evalStep (n - 1) s'
                
    addHistory res = lift . modify $ \set -> set { history = res : history set }

    addbp xs = case xs of
        "rule": rs -> case reads (unwords rs) of 
            (r', "") : _ -> do
                outputStrLn "ok"
                lift . modify $ \set -> set { breakPoints = AtRule r' : breakPoints set }
            _ -> outputStrLn "can't parse that rule"
        ["call", f] -> do
            outputStrLn "ok"
            lift . modify $ \set -> set { breakPoints = AtCall f : breakPoints set }
        _ -> outputStrLn "Sirisly u want m3 too parse that?"

    printSummary st = do
        hist <- lift (gets history)
        case st of
            StgState {}   -> return ()
            OmegaState {} -> outputStrLn "Omega:"
            PsiState {}   -> outputStrLn "Psi:"
            IrrState {}   -> outputStrLn "Irr:"
        case hist of
            [] -> do
                outputStrLn $ "Rule: " ++ show RInitial
                printCode  $ code  st
                printCStack $ cstack st
                outputStrLn $ "heap("  ++ show (M.size $ heap st) ++ ")"
                loop st
            (rule, st) : _ -> do
                outputStrLn $ "Rule: " ++ show rule
                printCode   $ code  st
                printCStack  $ cstack st
                outputStrLn $ "heap("  ++ show (M.size $ heap st) ++ ")"
                outputStrLn $ "astack\n" ++ show (prAStack PP.text $ astack st)
                loop st
                

    loopView st xs = do
        case xs of
            ["h"]    -> printHeap (heap st)
            ["heap"] -> printHeap (heap st)
            "heap":fs -> mapM_ (printHeapLookup (heap st)) fs
            "h":fs -> mapM_ (printHeapLookup (heap st)) fs
            ["set"]  -> printSetting =<< lift (gets settings)
            ["settings"] -> printSetting =<< lift (gets settings)
            ["s"]     -> printCStack (cstack st)
            ["stack"] -> printCStack (cstack st)
            ["c"]     -> printCode (code st)
            ["code"]  -> printCode (code st)
            ["rules"] -> printRules
            ["sum"]   -> printSummary st
            ["bp"]    -> do
                bps <- lift (gets breakPoints)
                outputStrLn $ show bps
            _ -> outputStrLn "wouldn't we all?"
        loop st

    printSetting set = do
        outputStrLn "Current Settings:"
        mapM_ (outputStrLn . show) 
            [ "steping" ~> steping set
            , "quiet"    ~> quiet set
            , "forceStg" ~> forceStg set
            , "toGC"     ~> toGC set
            ]
      where
        (~>) = (,)

    printCode code = outputStrLn $ "code: " ++ show (prExpr PP.text code)

    printCStack cstack = outputStrLn $ "stack(" ++ show (length cstack) ++ "):\n" 
                                  ++ show (prCStack PP.text cstack) 

    printHeapLookup heap f = outputStrLn $ printHeapFunctions $ M.filterWithKey (\k _ -> k == f) heap

    printHeap heap = outputStrLn $ "heap(" ++ show (M.size heap) ++ "): \n" 
                                ++ printHeapFunctions heap

    printHeapFunctions :: Heap String -> String
    printHeapFunctions heap = concat 
        [ padFunction (id ++ if loc == OnAbyss then "!" else ""
                      , show $ prObj PP.text obj)
        | (id, (obj, loc)) <- H.toList heap]
      where
        padFunction :: (String, String) -> String
        padFunction (id, obj) = unlines . map ("  " ++) $ case lines obj of
            []     -> []
            x : xs -> (id ++ " = " ++ x) 
                        : map (\y -> replicate (length id + 3) ' ' ++ y) xs

            
    printRules = do
        ruls <- lift $ gets history
        outputStrLn $ "number of rules: " ++ show (length ruls)
        outputStrLn . show . map (\ list -> (head list, length list))
                    . group . sort . map fst $ ruls 

    printHelp = do
        mapM_ outputStrLn
            [ "Oh hi! This is the Interpreter speaking"
            , "Press the following to do the following"
            , ":q - to quit"
            , ":v or :view - to view something"
            , "   h or heap  - the heap"
            , "                accepts a list of heap objects to be printed"
            , "   s or stack - the stack"
            , "   c or code  - the code"
            , "   rules      - the number of rules fired"
            , "   sum        - a summary"
            , "   bp         - the current breakpoints"
            , ":bp - to add a new breakpoint"
            , "   rule <rule> - for that <rule>"
            , "   call <id>   - when calling <id>"
            , ":step <n> - step <n> steps :)"
            , ":back <n> - step <-n> steps (history)"
            , ":force! - forced evaulation"
            , ""
            , "Happy Hacking !!"
            ]

main :: IO ()
main = do
    -- First argument is the filename
    file:args <- getArgs
    print $ args
    case getOpt RequireOrder options args of
        (flags, [],      [])     -> do
             opts <- foldl (>>=) (return defaultSettings) flags
--             if forceStg opts 
--                then loadFile opts file >>= forceInterpreter >> return ()
--                else testInterpreter opts file
             testInterpreter opts file
        (_,     nonOpts, [])     -> error $ "unrecognized arguments: " ++ unwords nonOpts
        (_,     _,       msgs)   -> error $ concat msgs ++ usageInfo header options

data Flag = Version

options :: [OptDescr (Settings -> IO Settings)] 
options = 
    [ Option ['S'] ["step"] (ReqArg setSteping "BOOL") "step through"
    , Option ['F'] ["force"] (ReqArg setForce "Bool") "force evaluation"
--    , Option ['V'] ["visible"] (ReqArg setVisible "\"BOOL BOOL BOOL\"") "show code stack heap"
    , Option ['I'] ["integerinput"] 
        (ReqArg setInputInteger "Integer") 
        "single integer input"
    , Option ['L'] ["listinput"] 
        (ReqArg setInputIntegers "[Integer]") 
        "integer list input"
    ]


setSteping :: String -> Settings -> IO Settings
setSteping arg s = return $ s { steping = read arg }

{-
setVisible :: String-> Settings -> IO Settings
setVisible ind set = let (arg: arg2: arg3:_) = words ind
                         h = read arg3
                         s = read arg2
                         c = read arg
                     in return $ set {  showStgState =  stgState c s h }
-}
setForce :: String -> Settings -> IO Settings
setForce arg set = return $ set { forceStg = read arg }


setInputInteger :: String -> Settings -> IO Settings
setInputInteger arg s = 
    return $ s { lset = (lset s) {input = (input (lset s)) { inputInteger = Just (read arg) }}}

setInputIntegers :: String -> Settings -> IO Settings
setInputIntegers arg s = 
    return $ s { lset = (lset s) { input = (input (lset s)) { inputIntegers = Just (read arg) }}}


header = "Usage: main [OPTION...]"
