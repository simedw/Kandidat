module Parser.STGParser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Prim

import Data.Maybe
import Control.Monad

import Stg.AST


-- The reserved operators
operators :: [String]
operators = ["->", "==","_"]

-- The keywords
keywords :: [String]
keywords = ["let","letrec","in","case","of"
           ,"FUN","PAP","CON","THUNK","BLACKHOLE", "OPT"]

-- Creating the lexer
tok :: TokenParser st
tok = makeTokenParser LanguageDef
  { commentStart    = "{-"
  , commentEnd      = "-}"
  , commentLine     = "--"
  , nestedComments  = True
  , identStart      = letter   
  , identLetter     = alphaNum <|> oneOf "_'."
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
parseStg :: String -> Either ParseError [Function String]
parseStg = runParser program () ""

program :: P [Function String]
program = do 
    whiteSpace tok
    v <- semiSep tok scdef 
    eof
    return v

-- Parse an identifier
ident :: P String
ident = identifier tok


-- Parses a Supercombinator (function or CAF)
-- f = obj
scdef :: P (Function String)
scdef = do
    name <- ident
    reservedOp tok "="
    body <- object
    return $ Function name body

-- Parses an expression, which is expr' between infix operators.
-- (does this work?)
expr :: P (Expr String)
expr = buildExpressionParser table expr' 
  where
    table = map 
                (map (\prim -> op (toParsecAssoc prim) (opDesc prim))) 
            priorites

    op assoc s = flip Infix assoc $ do
        reservedOp tok s 
        return $ \e1 e2 ->
            case (e1,e2) of
                (EAtom a1,EAtom a2) -> ECall (Heap s) [a1,a2]
                -- This is very unfortunate that this function is not in the
                -- monad. I have to use the IO errors instead.
                _ -> error $ "expected atoms on both sides of " ++ s

-- The two levels of expressions
expr',expr2 :: P (Expr String)
expr' = app <|> letdef <|> casedef <|> expr2

expr2 = parens tok expr <|> EAtom `fmap` atom

-- Atoms, identifiers or integers. Extensible!
atom :: P (Atom String)
atom = (AVar . Heap) `fmap` ident 
   <|> numOrDec
   <|> ADec `fmap` float tok
-- <|> AChr `fmap` charLiteral tok
-- <|> AStr `fmap` stringLiteral tok

-- An integer or a floating point value
numOrDec :: P (Atom String)
numOrDec = do
    x <- naturalOrFloat tok
    return $ case x of
        Left  n -> ANum n
        Right f -> ADec f

-- Application
-- without:    f a1 .. an
app :: P (Expr String)
app = do
    name <- fmap Heap ident 
    args <- many atom
    case args of
        [] -> return (EAtom (AVar name))
        xs -> return (ECall name args)

-- Let and letrec definitions. Two different forms:
-- 1: let x = obj in e
-- 2: let { x1 = obj1 ; .. ; xn = objn } in e
letdef :: P (Expr String)
letdef = let' <|> letrec'
  where 
    let'    = do
        reserved tok "let" 
        defs <- braces tok (semiSep tok def) 
                 <|> ((:[]) `fmap` def)
        reserved tok "in"
        e <- expr
        return $ foldr (\(id, obj) e' -> ELet (NonRec id obj) e') e defs 
    letrec' = do
        reserved tok "letrec"
        defs <- braces tok (semiSep tok def) 
                 <|> ((:[]) `fmap` def)
        reserved tok "in"
        e <- expr
        return $ ELet (Rec defs) e
    def = do 
        lhs <- ident
        reservedOp tok "="
        obj <- object 
        return (lhs,obj) 
        
-- The default branch is here as
--      { x } -> e x
casedef :: P (Expr String)
casedef = do
    reserved tok "case"
    e <- expr
    reserved tok "of"
    brs <- braces tok (semiSep tok branch)
    return $ ECase e brs
  where
    branch = do
        name <- ident <|> (('*':) `liftM` braces tok ident)
        args <- many ident
        reservedOp tok "->"
        e <- expr 
        case name of
           '*':name' -> if null args then return (BDef name' e)
                                  else unexpected "arguments to default branch"
           _     -> return (BCon name args e)
                                

-- Object
object :: P (Obj String)
object = choice [fun, pap, con, thunk, opt, blackhole]
  where
    fun = do
        reserved tok "FUN"
        parens tok $ do 
            args <- many1 ident
            reservedOp tok "->"
            e <- expr
            return $ OFun args 0 e

    pap = do
        reserved tok "PAP"
        parens tok $ do
            o <- ident
            args <- many1 atom
            return $ OPap o args

    con = do
        reserved tok "CON"
        parens tok $ do
            name <- ident
            args <- many atom
            return $ OCon name args

    thunk = do
        reserved tok "THUNK"
        OThunk [] 0 `fmap` expr

    opt = do
        reserved tok "OPT"
        parens tok $ flip OOpt [] `fmap` atom

    blackhole = do 
        reserved tok "BLACKHOLE" 
        return OBlackhole
    
