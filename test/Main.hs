import Parser.Pretty.Test
import Test.QuickCheck

main = do
    sample (arbitrary :: Gen ParsedSTGAST)  
    putStrLn "This would be a test"
