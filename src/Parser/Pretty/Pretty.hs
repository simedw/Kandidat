module Parser.Pretty.Pretty where

import Stg.AST

import Text.PrettyPrint.ANSI.Leijen

prFun :: Function String -> Doc
prFun = prFunction text

prFuns = seppis . map prFun

-- prFunction :: (t -> Doc) -> Function t -> Doc
prFunction prVar (Function id obj) = var prVar id <+> equal <+> prObj prVar obj

-- prExpr :: (t -> Doc) -> Expr t -> Doc
prExpr prVar e = case e of
    EAtom atom -> prAtom prVar atom
    ECall id as -> var prVar id <+> hsep [ prAtom prVar a | a <- as ]
    ELet b binds e -> (key $ case b of
        True  -> "letrec"
        False -> "let" ) <$>  indent 4 (prLetBind prVar binds) 
                           <+> key "in" <+> prExpr prVar e
    ECase scrut binds -> key "case" <+> prExpr prVar scrut <+> key "of"
        <$> indent 4 (mkBrace $ map (prBind prVar) binds)
    EPop op as -> operator (show op) <> operator "#" <+> hsep [ prAtom prVar a | a <- as ]  

equal   = magenta equals
operator = magenta . text
key = yellow . text
conVar prVar  = bold . green . prVar
bindVar prVar = cyan . prVar
symbol        = red
otext = bold . blue . text
var   = (green .)
num   = green
mparens = enclose (symbol lparen) (symbol rparen)
mbraces = enclose (symbol lbrace) (symbol rbrace)

seppis = vcat . punctuate (text "" <$> symbol semi <+> text "")

mkBrace [] = symbol $ braces empty
mkBrace (x : xs) = symbol lbrace <+> x <$> mkBrace' xs
  where
    mkBrace' []     = symbol rbrace
    mkBrace' (x:xs) = symbol semi <+> x <$> mkBrace' xs

prBind prVar bind = case bind of
    BCon name args e -> conVar prVar name <+> hsep (map (bindVar prVar) args) <+> operator "->" 
         <+> prExpr prVar e
    BDef name e -> mbraces (bindVar prVar name) <+> operator "->" <+> prExpr prVar e

-- prBind :: (t -> Doc) -> [(t, Obj t)] -> Doc
prLetBind prVar binds = mkBrace
    [ bindVar prVar x <+> equal <+> prObj prVar obj
    | (x, obj) <- binds ]


-- prAtom :: (t -> Doc) -> Atom t -> Doc
prAtom prVar atom = case atom of
    AVar x -> var prVar x
    ANum n -> num $ integer n
    ADec f -> num $ double f

-- prObj :: (t -> Doc) -> Obj t -> Doc
prObj prVar obj = case obj of
    OFun args e -> otext "FUN" <+> mparens (hsep (map (bindVar prVar) args) <+> operator "->" 
                                          <+> prExpr prVar e)
    OPap obj args -> otext "PAP" <+> mparens (var prVar obj 
                                            <+> hsep (map (prAtom prVar) args))
    OCon name args -> otext "CON" <+> mparens (conVar prVar name 
                                             <+> hsep (map (prAtom prVar) args))
    OThunk e -> otext "THUNK" <+> mparens (prExpr prVar e)
    OOpt   a -> otext "OPT" <+> mparens (prAtom prVar a)
    OBlackhole -> otext "BLACKHOLE"
