{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternGuards #-}
module Interpreter where

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

import Parser.Diabetes
import Parser.Pretty.Pretty
import Parser.SugarParser
import Stg.AST
import Stg.GC
import Stg.Input
import Stg.Interpreter
import Stg.PrePrelude
import Stg.Rules
import Stg.Types

data Settings = Settings
  { steping      :: Bool
  , prelude      :: String
  , showStgState :: (StgState String -> String)
  , showStgRule  :: (Rule -> String)
  , input        :: Input
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
evalBP bp (rule, StgState code stack heap) = case bp of
    AtRule r -> r == rule
    AtCall id -> case code of 
        ECall fid _ -> id == fid

stgState :: Bool -> Bool -> Bool -> StgState String -> String
stgState sc ss sh st@(StgState {code, stack, heap}) = 
       showi sc "code" (show (prExpr PP.text code))
    ++ showi ss ("stack(" ++ show (length stack) ++ ")") (show stack) 
    ++ showi sh ("heap("++ show (M.size heap) ++")") (concat [ show (id, prObj PP.text obj) ++ "\n\t"
                             | (id, obj) <- M.toList heap])
  where
    showi :: Bool -> String -> String -> String
    showi True s d  = s ++ ": " ++ d ++ "\n"
    showi False s _ = s ++ "\n"



defaultSettings = Settings {
    steping      = True
  , prelude      = "Prelude.hls"
  , showStgState = stgState True True True
  , showStgRule  = show 
  , input        = defaultInput
  , quiet        = False
  , forceStg     = False
  , toGC         = True
  }

noheapSettings = defaultSettings { showStgState = stgState True True False }


data InterpreterState = IS
    { settings :: Settings
    , stgm     :: StgMState String
    , history  :: [Result]
    , breakPoints  :: [BreakPoint]
    }

-- forceInterpreter forces the return value to be evaluted
-- so we can display it in a nice fashion
-- current directory is the base directory to run `cabal test'
forceInterpreter :: Settings -> FilePath -> IO String
forceInterpreter settings file = do
    dir     <- getCurrentDirectory
    prelude <- readFile (dir </>  "prelude" </> prelude settings)
    res     <- readFile (dir </>  "testsuite" </> file)
    case parseSugar (res ++ "\n" ++ prelude) of
      Right fs -> let res = runForce (input settings) (prePrelude ++ run fs)
                   in return res
      Left  r  -> do return $ "fail: " ++ show r



testInterpreter :: Settings -> FilePath -> IO ()
testInterpreter set file = do
    dir     <- getCurrentDirectory
    prelude <- readFile (dir </> "prelude" </> prelude set)
    res     <- readFile (dir </> "testsuite" </> file)
    -- prelude must be last, otherwise parse error messages get wrong line numbers!
    case parseSugar (res ++ "\n" ++ prelude) of  
      Right fs -> do
        let st = initialState ( createGetFuns (input set)
                              ++ prePrelude
                              ++ run fs)
        evalStateT (Hl.runInputT Hl.defaultSettings (loop st)) IS
            { settings = set
            , stgm     = initialStgMState
            , history  = []
            , breakPoints = []
            } 
      Left  r  -> do putStrLn $ "fail: " ++ show r

loop :: StgState String -> InputT (StateT InterpreterState IO) ()
loop originalState  = do
    minput <- getInputLine "% " 
    set <- lift $ gets settings
    let st@(StgState code stack heap) = (if toGC set then gc else id) originalState
    case minput of
        Nothing -> return ()
        Just xs -> case words xs of
            [] -> evalStep 1 st
            [":q"]   -> return ()
            ":v" : xs    -> loopView st xs
            ":view" : xs -> loopView st xs
            ":bp"  : xs -> addbp xs >> loop st
            [":back", num] -> case reads num of
                ((x, "") : _) -> evalStep (negate x) st
                _ -> loop st
            [":step", num] -> case reads num of
                ((x, "") : _) -> evalStep x st
                _ -> loop st
            [":force"] -> forcing st >> loop st
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
    
    forcing st = do
        stg <- lift $ gets stgm
        outputStrLn . fst $ runState (force st) stg

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
        case runState (step s) stg of
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
        ["rule", r] -> case reads r of 
            (r', "") : _ -> do
                outputStrLn "ok"
                lift . modify $ \set -> set { breakPoints = AtRule r' : breakPoints set }
            _ -> outputStrLn "can't parse that rule"
        ["call", f] -> do
            outputStrLn "ok"
            lift . modify $ \set -> set { breakPoints = AtCall f : breakPoints set }
        _ -> outputStrLn "Sirisly u want m3 too parse that?"

    printSummary st@(StgState code stack heap) = do
        hist <- lift (gets history)
        case hist of
            [] -> do
                outputStrLn $ "Rule: " ++ show RInitial
                printCode code
                printStack stack
                outputStrLn $ "heap("  ++ show (M.size heap) ++ ")"
                loop st
            (rule, StgState code stack heap) : _ -> do
                outputStrLn $ "Rule: " ++ show rule
                printCode code
                printStack stack
                outputStrLn $ "heap("  ++ show (M.size heap) ++ ")"
                loop st
                

    loopView st@(StgState code stack heap) xs = do
        case xs of
            ["h"]    -> printHeap heap
            ["heap"] -> printHeap heap
            ["set"]  -> printSetting =<< lift (gets settings)
            ["settings"] -> printSetting =<< lift (gets settings)
            ["s"]     -> printStack stack
            ["stack"] -> printStack stack
            ["c"]     -> printCode code
            ["code"]  -> printCode code
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

    printCode code = do
        outputStrLn $ "code: " ++ show (prExpr PP.text code)

    printStack stack = do
        outputStrLn $ "stack(" ++ show (length stack) ++ "): " ++ show stack 

    printHeap heap = do
        outputStrLn $ "heap(" ++ show (M.size heap) ++ "): \n" 
                    ++ concat [ foo (id, show $ prObj PP.text obj)
                              | (id, obj) <- M.toList heap]
      where
        foo :: (String, String) -> String
        foo (id, obj) = unlines . map ("  " ++) $ case lines obj of
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
            , "   s or stack - the stack"
            , "   c or code  - the code"
            , "   rules      - the number of rules fired"
            , "   sum        - a summary"
            , "   bp         - the current breakpoints"
            , ":bp - to add a new breakpoint"
            , "   rule <rule> - for that <rule>"
            , "   call <id>   - when calling <id>"
            , "step <n> - step <n> steps :)"
            , "back <n> - step <-n> steps (history)"
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
             testInterpreter opts file
        (_,     nonOpts, [])     -> error $ "unrecognized arguments: " ++ unwords nonOpts
        (_,     _,       msgs)   -> error $ concat msgs ++ usageInfo header options

data Flag = Version

options :: [OptDescr (Settings -> IO Settings)] 
options = 
    [ Option ['S'] ["step"] (ReqArg setSteping "BOOL") "step through"
    , Option ['F'] ["force"] (ReqArg setForce "Bool") "force evaluation"
    , Option ['V'] ["visible"] (ReqArg setVisible "\"BOOL BOOL BOOL\"") "show code stack heap"
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
setVisible ind set = let (arg: arg2: arg3:_) = words ind
                         h = read arg3
                         s = read arg2
                         c = read arg
                     in return $ set {  showStgState =  stgState c s h }

setForce :: String -> Settings -> IO Settings
setForce arg set = return $ set { forceStg = read arg }


setInputInteger :: String -> Settings -> IO Settings
setInputInteger arg s = 
    return $ s { input = (input s) { inputInteger = Just (read arg) }}

setInputIntegers :: String -> Settings -> IO Settings
setInputIntegers arg s = 
    return $ s { input = (input s) { inputIntegers = Just (read arg) }}


header = "Usage: main [OPTION...]"
