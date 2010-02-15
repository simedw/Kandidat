module Test.Interpreter where

import Parser.SugarParser
import Parser.Diabetes
import Stg.Interpreter
import System.Directory
import System.FilePath

testParser :: FilePath -> IO ()
testParser file = do
    dir     <- getCurrentDirectory
    prelude <- readFile (dir </> ".." </> "prelude" </> "Prelude.hls")
    res     <- readFile (dir </> ".." </> "testsuite" </> file)
    case parseSugar (prelude ++ res) of
      Right fs -> mapM_ (\x -> putStrLn "#####" >> getChar >> print x) $ eval (map run fs)
      Left  r  -> do putStr $ "fail: " ++ show r

