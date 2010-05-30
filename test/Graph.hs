{-# LANGUAGE GADTs #-}
module Graph where

import Data.Char (isSpace, isNumber)

import System.Environment
import System.Exit

import Stg.Interpreter
import Stg.AST
import Stg.Input
-- import Parser.SugarParser
import Util


getList :: Bool -> [Double] -> String -> IO [Double]
getList optOrNot2Opt dls expr = do
    rexp <- case lexx expr of
        Nothing -> do
                putStrLn "lex failure"
                exitWith (ExitFailure 1)
        Just lexpr -> case parsePExpr lexpr of
            Left err -> do
                putStrLn err
                exitWith (ExitFailure 1)
            Right e -> return (pretty e)
    
    f <- flip loadFile "Graph.hls" $ flip (LSettings "Prelude.hls") optOrNot2Opt defaultInput
        { inputDoubles = Just dls
        , inputString = Just rexp
        }

    case forceInterpreter f of
        Left err -> error err
        Right res -> return (convert res)
             

convert (SCon "Cons" [x, xs]) = convertD x : convert xs
convert _                     = []

convertD :: SValue String -> Double
convertD (SCon "D#" [SAtom (ADec d)]) = d

data PExpr
    = Bin String PExpr PExpr
    | Uno String PExpr
    | Var
    | Lit Double

pretty :: PExpr -> String
pretty e = unwords (pretty' e)

pretty' :: PExpr -> [String]
pretty' e = case e of
    Bin s e1 e2 -> pretty' e1 ++ pretty' e2 ++ [s]
    Uno f e     -> pretty' e  ++ [f]
    Var       -> ["x"]
    Lit d     -> [show d]

data Tok
    = TX
    | TSin
    | TCos
    | TAdd
    | TSub
    | TMul
    | TDiv
    | TOpenP
    | TCloseP
    | TLit Double
    deriving (Eq, Show)

lexx :: String -> Maybe [Tok]
lexx str = case dropWhile isSpace str of
    [] -> return []
    's':'i':'n':' ' :rest -> (TSin :) `fmap` lexx rest
    's':'i':'n':'(' :rest -> (\xs -> TSin : TOpenP : xs) `fmap` lexx rest
    'c':'o':'s':' ' :rest -> (TCos :) `fmap` lexx rest
    'c':'o':'s':'(' :rest -> (\xs -> TCos : TOpenP : xs) `fmap` lexx rest
    '+' : rest -> (TAdd :) `fmap` lexx rest
    '-' : rest -> (TSub :) `fmap` lexx rest
    '*' : rest -> (TMul :) `fmap` lexx rest
    '/' : rest -> (TDiv :) `fmap` lexx rest
    '(' : rest -> (TOpenP :) `fmap` lexx rest
    ')' : rest -> (TCloseP :) `fmap` lexx rest
    'x' : rest -> (TX :) `fmap` lexx rest
    str' -> let (mnum, rest) = break (\x -> not $ isNumber x || x == '.') str'
        in case filter (null . snd) $ reads mnum of
                [(d, _)] -> (TLit d :) `fmap` lexx rest
                _ -> Nothing

type ParseResult s a = [(a, [s])]

data P s a where
  Fail   :: P s a
  ReturnChoice :: a -> P s a -> P s a -- ReturnChoice x p == return x +++ p
  SymbolBind :: (s -> P s a) -> P s a -- SymbolBind f     == symbol >>= f

symbol :: P s s
symbol = SymbolBind return

pfail :: P s a
pfail  = Fail

(+++) :: P s a -> P s a -> P s a
SymbolBind f     +++ SymbolBind g     = SymbolBind (\x -> f x +++ g x)
Fail             +++ q                = q
p                +++ Fail             = p
ReturnChoice x p +++ q                = ReturnChoice x (p +++ q)
p                +++ ReturnChoice x q = ReturnChoice x (p +++ q)

instance Monad (P s) where
  return x = ReturnChoice x pfail
  Fail             >>= f = Fail
  -- Use L1 and L5 to derive this clause
  ReturnChoice x p >>= f = f x +++ (p >>= f)
  SymbolBind k     >>= f = SymbolBind (\x -> k x >>= f)

sat :: (s -> Bool) -> P s s
sat p = do
  x <- symbol
  if p x then return x
         else pfail

this :: Eq s => s -> P s s
this x = sat (x ==)

chainLeft :: P s (a -> a -> a) -> P s a -> P s a
chainLeft op term = do
    e <- term
    chain e
  where
    chain e = return e +++ do
      o  <- op
      e' <- term
      chain (e `o` e')

parse :: P s a -> [s] -> ParseResult s a
parse (SymbolBind f) (c : s) = parse (f c) s
parse (SymbolBind f) []      = []
parse Fail       _           = []
parse (ReturnChoice x p) s   = (x, s) : parse p s

parsePExpr :: [Tok] -> Either String PExpr
parsePExpr s = case filter (null . snd) $ parse expr s of
    [(a, _)] -> Right a
    _        -> Left "Parse Error"

type PP = P Tok PExpr

expr :: PP
expr = chainLeft plusP termP
    where
      plusP = do { this TAdd ; return (Bin "+") }
         +++  do { this TSub ; return (Bin "-") }

termP :: PP
termP = chainLeft mulP facP
  where
    mulP = do { this TMul ; return (Bin "*") }
       +++ do { this TDiv ; return (Bin "/") }

facP :: PP
facP = sinus +++ expr'

expr' :: PP
expr' = do { this TX ; return Var } 
    +++ dblP
    +++ do { this TOpenP 
           ; e <- expr
           ; this TCloseP 
           ; return e}

sinus :: PP
sinus = do { this TSin ; e <- expr' ; return $ Uno "sin" e}
    +++ do { this TCos ; e <- expr' ; return $ Uno "cos" e}

dblP :: PP
dblP = do 
    tok <- symbol
    case tok of
        TLit d -> return $ Lit d
        _      -> pfail 
