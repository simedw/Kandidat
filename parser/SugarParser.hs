module STGParser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Prim

import Data.Maybe
import Data.Char
import Control.Monad

import SugarTree


-- The reserved operators
operators :: [String]
operators = ["->", "==","_"]

-- The keywords
keywords :: [String]
keywords = ["let","letrec","in","case","of"]

-- Creating the lexer
tok :: TokenParser st
tok = makeTokenParser LanguageDef
  { commentStart    = "{-"
  , commentEnd      = "-}"
  , commentLine     = "--"
  , nestedComments  = True
  , identStart      = letter   
  , identLetter     = alphaNum <|> oneOf "_'"
  , opStart         = oneOf "+-*/%<>=_"
  , opLetter        = oneOf "+-*/%<>=_"
  , reservedNames   = keywords
  , reservedOpNames = operators
  , caseSensitive   = True
  }

-- The Parser type
-- The state is Nothing if not parsing sugar,
-- otherwise it is Just the next integer in the name supply.
type P = GenParser Char ()

-- Run the parser
parseSugar :: String -> Either ParseError [Function String]
parseSugar = runParser program () ""

program :: P [Function String]
program = do 
    whiteSpace tok
    v <- semiSep tok scdef 
    eof
    return v

-- Parse an identifier
ident :: P String
ident = identifier tok

headIs :: (a -> Bool) -> [a] -> Bool
headIs f (x:xs) = f x
headIs _ []     = False

lident :: P String
lident = do
    s <- identifier tok
    if headIs isLower s
        then return s
        else unexpected "expected lower-case identifier"

-- Parses a Supercombinator (function or CAF)
-- with sugar: f x1 .. xn = expr 
-- without:    f = obj
scdef :: P (Function String)
scdef = do
    name <- lident
    args <- many lident
    reservedOp tok "="
    body <- expr
    return $ Function name args body 

-- Parses an expression, which is expr' between infix operators.
expr :: P (Expr String)
expr = buildExpressionParser table expr'
  where
    table = [ map (op AssocLeft) ["*","/","%"]
            , map (op AssocLeft) ["+","-"]
            , map (op AssocNone) ["<","==",">"]
            ]

    op assoc s = flip Infix assoc $ do
        reservedOp tok s 
        return $ \e1 e2 -> ECall s [e1,e2]

-- The two levels of expressions
expr',expr2 :: P (Expr String)
expr' = app <|> letdef <|> casedef <|> expr2

expr2 = parens tok expr <|> (EAtom `fmap` atom)

-- Atoms, identifiers or integers. Extensible!
atom :: P (Atom String)
atom = AVar `fmap` ident
   <|> ANum `fmap` natural tok
-- <|> ADec `fmap` float tok
-- <|> AChr `fmap` charLiteral tok
-- <|> AStr `fmap` stringLiteral tok

-- Singlevariable, application or constructor!
-- with sugar: f e1 .. en
app :: P (Expr String)
app = do
    name <- ident 
    args <- many expr2
    return $ mkApp name args 
  where
    mkApp s@(x:xs) [] | isLower x = EAtom (AVar s) 
    mkApp s@(x:xs) es | isLower x = ECall s es
    mkApp s        es = ECon s es

letdef :: P (Expr String)
letdef = rest =<< (let' <|> letrec')
  where 
    let'    = reserved tok "let" >> return False
    letrec' = reserved tok "letrec" >> return True
    rest b  = do
        defs <- braces tok (semiSep tok def) 
                 <|> ((:[]) `fmap` def)
        reserved tok "in"
        e <- expr
        return $ ELet b defs e
    def = do 
        lhs  <- lident
        args <- many lident
        reservedOp tok "="
        rhs <- expr 
        return (lhs,args,rhs) 

casedef :: P (Expr String)
casedef = do
    reserved tok "case"
    e <- expr
    reserved tok "of"
    brs <- braces tok (semiSep tok branch)
    return $ ECase e brs
  where
    branch = do
        name <- ident
        args <- many lident
        reservedOp tok "->"
        e <- expr 
        if headIs isLower name
            then if null args then return (BDef name e)
                              else unexpected "arguments to default branch"
            else return (BCon name args e)
