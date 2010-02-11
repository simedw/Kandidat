import Parser.STGParser
import Parser.Pretty.Test

import System.Directory
import System.FilePath

import Test.QuickCheck

type TIO = IO TestResult

data Test = Test 
  { name   :: String
  , action :: TIO
  }

data TestResult
  = Ok
  | Fail


runTests :: [Test] -> IO ()
runTests tests = do
  suc <- runTests' 0 tests
  pline
  putStrLn $ "summary"
  pline
  putStrLn $ "of " ++ show (length tests) ++ ", " ++ show suc ++ " succeded"
  where
    title = "running: "
    pline = putStrLn $ replicate 68 '–'

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

main = runTests
  [ Test { name = "Testing parser", action = testParser}
  , Test { name = "QuickCheck pretty", action = qcPretty}
  , Test { name = "QuickCheck pretty 2", action = qcPretty}
  ]

testParser :: TIO
testParser = do
  dir <- getCurrentDirectory
  let filename = dir </> "src" </> "Examples" </> "noSugar.stg"
  file <- readFile filename
  case parseStg file of
    Right _ -> do
      putStrLn $ "ok"
      return Ok
    Left r  -> do
      putStrLn $ "fail: " ++ show r
      return Fail

qcPretty :: TIO
qcPretty = do 
  res <- quickCheckResult prop_Pretty_Parse
  case res of
    Success l -> return Ok
    f -> print f >> return Fail
