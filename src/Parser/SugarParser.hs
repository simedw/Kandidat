module Parser.SugarParser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Prim

import Data.Maybe
import Data.Char
import Control.Monad
import Control.Applicative hiding ((<|>), many)

import Parser.SugarTree

import Shared.Primitives


-- The reserved operators
operators :: [String]
operators = ["->", "=",";","\\",".",".."]

-- The keywords
keywords :: [String]
keywords = ["let","letrec","in","case","of","optimise","with"]

-- Creating the lexer
tok :: TokenParser st
tok = makeTokenParser LanguageDef
  { commentStart    = "{-"
  , commentEnd      = "-}"
  , commentLine     = "--"
  , nestedComments  = True
  , identStart      = letter   
  , identLetter     = alphaNum <|> oneOf "_'"
  , opStart         = oneOf "+-*/%<>=_$.:&|"
  , opLetter        = oneOf "+-*/%<>=_$.:&|"
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
    table = [ map (op AssocRight) [".",".."]
            , map (op AssocRight) [":","++"]
            ]
         ++ map 
                (map (\prim -> op (toParsecAssoc prim) (opDesc prim))) 
                priorities
         ++ [ map (op AssocRight) ["&&"]
            , map (op AssocRight) ["||"]
            , map (dollar AssocRight) ["$"]
            ]

    aliases = [("&&","and")
              ,("||","or")
              ,("++","append")
              ,(":" ,"cons")
              ,("." ,"compose")
              ,("..","dot")
              ]

    op assoc s = flip Infix assoc $ do
        reservedOp tok s 
        return $ \e1 e2 -> ECall (fromMaybe s (lookup s aliases)) [e1,e2]
        
    dollar assoc s = flip Infix assoc $ do
        reservedOp tok s
        return $ \e1 e2 -> case e1 of
            EAtom (AVar i) -> ECall i [e2]
            ECall i args   -> ECall i $ args ++ [e2]
            _              -> error "Weird $" -- alternatively ECall s [e1,e2]

-- The two levels of expressions
expr',expr2 :: P (Expr String)
expr' = opt <|> app <|> letdef <|> casedef <|> lambda <|> expr2

expr2 = parens tok afterparensExpr <|> atomExpr

afterparensExpr :: P (Expr String)
afterparensExpr = (flip ECall [] `fmap` operator tok) <|> expr

-- Atoms, identifiers or integers. Extensible!
atomExpr :: P (Expr String)
atomExpr = EAtom  `fmap` (try signedIntOrFloat)
-- <|> ADec `fmap` float tok
   <|> do (EAtom . AChr) `fmap` (try $ charLiteral tok) 
   <|> do try stringParser
-- <|> AStr `fmap` stringLiteral tok
   <|> do i <- ident 
          if headIs isLower i
              then return (EAtom (AVar i))
              else return (ECon i [])


stringParser :: P (Expr String)
stringParser = (EAtom . AStr) `fmap` stringLiteral tok  

-- Parsec allows whitespace between - and the number, as in "- 1".
-- We cannot tolerate this! "-1" is negative one, and "x - 1" is x minus 1.
-- Therefore all this low-level parser stuff
signedIntOrFloat :: P (Atom String)
signedIntOrFloat = do
    sign  <- (char '-' >> return (negate, negate)) <|> return (id, id)
    x <- naturalOrFloat tok
    return $ case x of
        Left  n -> ANum $ (fst sign) n
        Right f -> ADec $ (snd sign) f

-- optimisation expression
opt :: P (Expr String)
opt = do
    reserved tok "optimise"
    e <- expr
    args <- with <|> return []
    return $ EOpt e args
  where
    with    = reserved tok "with" >> braces tok (semiSep tok setting)
    setting = do
        s <- ident
        case s of
            "inlinings" -> Inlinings `fmap` expr
            "noinlinings" -> return $ Inlinings $ EAtom $ ANum 0
            "inline" -> do
               f <- lident
               e <- expr
               return $ Inline f e
            "noinline" -> flip Inline (EAtom $ ANum 0) `fmap` lident
            "casebranches" -> return CaseBranches
            _ -> unexpected $ s ++ " as argument in with"
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
