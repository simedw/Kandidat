module Parser.SugarParser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Prim

import Data.Maybe
import Data.Char
import Control.Monad

import Parser.SugarTree


-- The reserved operators
operators :: [String]
operators = ["->", "=",";","\\","."]

-- The keywords
keywords :: [String]
keywords = ["let","letrec","in","case","of","optimise"]

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
    v <- endBy scdef (reservedOp tok ";")
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
            , map (op AssocNone) ["<","<=","==",">=",">"]
            ]

    op assoc s = flip Infix assoc $ do
        reservedOp tok s 
        return $ \e1 e2 -> ECall s [e1,e2]

-- The two levels of expressions
expr',expr2 :: P (Expr String)
expr' = opt <|> app <|> letdef <|> casedef <|> lambda <|> expr2

expr2 = parens tok afterparensExpr <|> atomExpr

afterparensExpr :: P (Expr String)
afterparensExpr = (flip ECall [] `fmap` operator tok) <|> expr

-- Atoms, identifiers or integers. Extensible!
atomExpr :: P (Expr String)
atomExpr = (EAtom . ANum) `fmap` (try signedInt)
-- <|> ADec `fmap` float tok
-- <|> AChr `fmap` charLiteral tok
-- <|> AStr `fmap` stringLiteral tok
   <|> do i <- ident 
          if headIs isLower i
              then return (EAtom (AVar i))
              else return (ECon i [])
 
-- Parsec allows whitespace between - and the number, as in "- 1".
-- We cannot tolerate this! "-1" is negative one, and "x - 1" is x minus 1.
-- Therefore all this low-level parser stuff
signedInt :: P Integer
signedInt = do
    sign <- (char '-' >> return negate) <|> return id
    n <- many1 (oneOf ['0'..'9']) -- nat
    whiteSpace tok
    return $ sign $ read n

-- for debugging :)
fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "fromRight: isLeft"

-- optimisation expression
opt :: P (Expr String)
opt = do
    reserved tok "optimise"
    e <- expr
    return $ EOpt e 

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

lambda :: P (Expr String)
lambda = do
    reservedOp tok "\\"
    args <- many lident
    reservedOp tok "."
    body <- expr
    return $ ELam args body
    

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
