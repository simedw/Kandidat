module Parser.Pretty.Pretty where

import Stg.AST

import Text.PrettyPrint

prFun :: Function String -> Doc
prFun = prFunction text

prFuns = seppis . map prFun

-- prFunction :: (t -> Doc) -> Function t -> Doc
prFunction prVar (Function id obj) = prVar id <+> equals <+> prObj prVar obj

-- prExpr :: (t -> Doc) -> Expr t -> Doc
prExpr prVar e = case e of
    EAtom atom -> prAtom prVar atom
    ECall id as -> prVar id <+> hsep [ prAtom prVar a | a <- as ]
    ELet b binds e -> (text $ case b of
        True  -> "letrec"
        False -> "let" )<+> braces (nest 0 (prLetBind prVar binds)) $$ text "in" <+> prExpr prVar e
    ECase scrut binds -> text "case" <+> prExpr prVar scrut <+> text "of"
        <+> braces (text "" $+$ nest 2 (seppis $ map (prBind prVar) binds))
    EPop op as -> text (show op) <+> text "#" <+> hsep [ prAtom prVar a | a <- as ]  

seppis = vcat . punctuate semi

prBind prVar bind = case bind of
    BCon name args e -> prVar name <+> hsep (map prVar args) <+> text "->" 
         <+> prExpr prVar e
    BDef name e -> braces (prVar name) <+> text "->" <+> prExpr prVar e

-- prBind :: (t -> Doc) -> [(t, Obj t)] -> Doc
prLetBind prVar binds = seppis
    [ prVar x <+> equals <+> prObj prVar obj
    | (x, obj) <- binds ]

-- prAtom :: (t -> Doc) -> Atom t -> Doc
prAtom prVar atom = case atom of
    AVar x -> prVar x
    ANum n -> integer n

-- prObj :: (t -> Doc) -> Obj t -> Doc
prObj prVar obj = case obj of
    OFun args e -> text "FUN" <+> parens (hsep (map prVar args) <+> text "->" 
                                          <+> prExpr prVar e)
    OPap obj args -> text "PAP" <+> parens (prVar obj 
                                            <+> hsep (map (prAtom prVar) args))
    OCon name args -> text "CON" <+> parens (prVar name 
                                             <+> hsep (map (prAtom prVar) args))
    OThunk e -> text "THUNK" <+> parens (prExpr prVar e)
    OOpt   a -> text "OPT" <+> parens (prAtom prVar a)
    OBlackhole -> text "BLACKHOLE"

