module STGParser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Prim

import Data.Maybe
import Control.Monad

import AST


-- The reserved operators
operators :: [String]
operators = ["->", "==","_"]

-- The keywords
keywords :: [String]
keywords = ["let","letrec","in","case","of"
           ,"FUN","PAP","CON","THUNK","BLACKHOLE"]

-- Creating the lexer
tok :: TokenParser st
tok = makeTokenParser LanguageDef
  { commentStart    = "{-"
  , commentEnd      = "-}"
  , commentLine     = "--"
  , nestedComments  = True
  , identStart      = letter   <|> char '_'
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
type P = GenParser Char (Maybe Integer) 

-- Run the parser
parseStg,parseStgSugar :: String -> Either ParseError [Function String]
parseStg      = runParser program Nothing  ""
parseStgSugar = runParser program (Just 0) ""

program :: P [Function String]
program = do 
    whiteSpace tok
    v <- semiSep tok scdef 
    eof
    return v

-- Parse an identifier
ident :: P String
ident = identifier tok

-- A new (string) name from the name supply
newName :: P String
newName = do
    (Just x) <- getState
    setState (Just (x+1))
    return ("t." ++ show x)

-- Do the first parse if sugar is on, otherwise the second
caseSugar :: P a -> P a -> P a
caseSugar m1 m2 = do 
    s <- getState 
    case s of (Just _) -> m1
              Nothing  -> m2

-- Parses a Supercombinator (function or CAF)
-- with sugar: f x1 .. xn = expr 
-- without:    f = obj
scdef :: P (Function String)
scdef = do
    name <- ident
    caseSugar 
        (do 
            vars <- many ident
            reservedOp tok "="
            body <- expr
            return $ Function name (OFun vars body)) 
        (do
            reservedOp tok "="
            body <- object
            return $ Function name body)

-- Parses an expression, which is expr' between infix operators.
expr :: P (Expr String)
expr = buildExpressionParser table expr'
  where
    table = [ map (op AssocLeft) ["*","/","%"]
            , map (op AssocLeft) ["+","-"]
            , map (op AssocNone) ["<","==",">"]
            ]

    -- unfortunately op returns a pure function, so I have to use the 
    -- name-supply without knowing if they are going to be used!
    op assoc s = flip Infix assoc $ do
        reservedOp tok s 
        names <- replicateM 2 newName
        return $ \e1 e2 -> 
            let (args,defs) = unzip $ zipWith mkDef [e1,e2] names 
            in  eLet False (catMaybes defs) $ ECall s args

    mkDef (EAtom e) n = (e,Nothing)
    mkDef e         n = (AVar n,Just (n,OThunk e))
        

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

-- Application
-- with sugar: f e1 .. en
--    becomes: let { a1 = THUNK e1 ; ..
--                   an = THUNK en }
--             in f a1 .. an
--             (but not for ei that are atoms already)
-- without:    f a1 .. an
app :: P (Expr String)
app = do
    name <- ident 
    caseSugar
        (do 
            args <- many expr2
            case args of
                [] -> return (EAtom (AVar name))
                xs -> do
                    (args',defs) <- heapNonAtoms args
                    return $ eLet False (catMaybes defs)
                           $ ECall name args')
        (do 
            args <- many atom
            case args of
                [] -> return (EAtom (AVar name))
                xs -> return (ECall name args))

-- Takes a list of expressions, some may be atoms.
-- Makes a definition list, with atoms in lhs and in 
-- rhs maybe an allocation to an object, if it was not
-- an atom in the original list.
heapNonAtoms :: [Expr String] -> P ([Atom String], [Maybe (String, Obj String)])
heapNonAtoms = mapAndUnzipM $ \e ->
    case e of
         (EAtom a) -> return (a,Nothing)
         _         -> do
             n <- newName
             return (AVar n,Just (n,OThunk e))

-- If the list is non-empty, return an ELet element
eLet :: Bool -> [(t, Obj t)] -> Expr t -> Expr t
eLet _ [] = id
eLet b xs = ELet b xs

-- Let and letrec definitions. Two different forms:
-- 1: let x = obj in e
-- 2: let { x1 = obj1 ; .. ; xn = objn } in e
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
        lhs <- ident
        reservedOp tok "="
        obj <- object 
        return (lhs,obj) 
        
casedef :: P (Expr String)
casedef = do
    reserved tok "case"
    e <- expr
    reserved tok "of"
    brs <- braces tok (semiSep tok branch)
    return $ ECase e brs
  where
    branch = do
        name <- ident <|> (reservedOp tok "_" >> return "_")
        args <- many ident
        reservedOp tok "->"
        e <- expr 
        case name of
            "_" -> if null args then return (BDef  name e)
                                else unexpected "arguments to default branch"
            _   -> return (BCon name args e)
                                

-- Object
-- Still to implement: CON with non-atom arguments.
-- This is a bit tricky, because then the return type is Expr and not Obj.
object :: P (Obj String)
object = choice [fun, pap, con, thunk, blackhole]
  where
    fun = do
        reserved tok "FUN"
        parens tok $ do 
            args <- many1 ident
            reservedOp tok "->"
            e <- expr
            return $ OFun args e

    pap = do
        reserved tok "PAP"
        parens tok $ do
            o <- object
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
        OThunk `fmap` expr

    blackhole = do 
        reserved tok "BLACKHOLE" 
        return OBlackhole
    
