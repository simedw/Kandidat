{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Parser.Pretty.Pretty where

import Unsafe.Coerce

import Stg.AST
import Stg.Types
import Stg.Stack

import Text.PrettyPrint.ANSI.Leijen

data Syntax t = Syntax 
    { equal    :: Doc
    , operator :: String -> Doc
    , key      :: String -> Doc
    , conVar   :: t -> Doc
    , bindVar  :: t -> Doc
    , symbol   :: Doc -> Doc
    , object   :: String -> Doc
    , var      :: t -> Doc
    , num      :: Doc -> Doc
    , chr      :: Char -> Doc
    }

syntaxNormal :: (t -> Doc) -> Syntax t
syntaxNormal prVar = Syntax
    { equal    = equals
    , operator = text
    , key      = text
    , conVar   = prVar
    , bindVar  = prVar
    , symbol   = id
    , object   = text
    , var      = prVar
    , num      = id
    , chr      = char
    }

syntaxColour :: (t -> Doc ) -> Syntax t
syntaxColour prVar = Syntax 
    { equal    = magenta equals
    , operator = magenta . text
    , key      = yellow . text
    , conVar   = bold . green . prVar
    , bindVar  = cyan . prVar
    , symbol   = red
    , object   = bold . blue . text
    , var      = green . prVar
    , num      = green
    , chr      = green . char
    }

data PPrinters t = PPrinters
    { ppFun   :: Function t -> Doc
    , ppExpr  :: Expr t     -> Doc
    , ppObj   :: Obj t      -> Doc
    , ppAtom  :: Atom t     -> Doc
    , ppCStack :: ContStack t    -> Doc
    , ppAStack :: ArgStack t    -> Doc
    }

mkC = mkPretty . syntaxColour
mkN = mkPretty . syntaxNormal

prFun :: Function String -> Doc
prFun =  ppFun $ mkN text

prFuns = seppis (syntaxNormal text) . map prFun

ppFunctionN = ppFun . mkN
ppFunction  = ppFun . mkC

prExprN = ppExpr . mkN
prExpr  = ppExpr . mkC

prObjN = ppObj . mkN
prObj  = ppObj . mkC

prAtomN = ppAtom . mkN
prAtom  = ppAtom . mkC

prCStackN = ppCStack . mkN
prCStack  = ppCStack . mkC

prAStackN = ppAStack . mkN
prAStack  = ppAStack . mkC

seppis syn = vcat . punctuate (text "" <$> symbol syn semi <+> text "")

mkPretty :: forall t. Show t => Syntax t -> PPrinters t
mkPretty (Syntax {..})  = PPrinters {..}
  where
    ppFun :: Function t -> Doc
    ppFun (Function id obj) = var id <+> equal <+> ppObj obj
    
    ppExpr :: Expr t -> Doc
    ppExpr e = case e of
        EAtom atom -> ppAtom atom
        ECall id as -> ppVar id <+> hsep [ ppAtom a | a <- as ]
        ELet binds e -> (key $ case isRecursive binds of
            True  -> "letrec"
            False -> "let" ) <$>  indent 4 (ppLetBind binds) 
                               <+> key "in" <+> hang 1 (ppExpr e)
        ECase scrut binds -> key "case" <+> ppExpr scrut <+> key "of"
            <$> indent 4 (mkBrace $ map ppBranch binds)
        EPop op as -> operator (show op) <> operator "#" 
                        <+> hsep [ ppAtom a | a <- as ]  
        ESVal sval -> ppSVal sval
    
    mparens, mbraces :: Doc -> Doc
    mparens = enclose (symbol lparen) (symbol rparen)
    mbraces = enclose (symbol lbrace) (symbol rbrace)
    mbrackets = enclose (symbol lbracket) (symbol (rbracket))
    
    
    mkBrace :: [Doc] -> Doc
    mkBrace [] = symbol $ braces empty
    mkBrace (x : xs) = symbol lbrace <+> x <$> mkBrace' xs
      where
        mkBrace' []     = symbol rbrace
        mkBrace' (x:xs) = symbol semi <+> x <$> mkBrace' xs
   
    ppBranch :: Branch t -> Doc 
    ppBranch branch = case branch of
        BCon name args e -> conVar name <+> hsep (map bindVar args) <+> operator "->" 
             <+> ppExpr e
        BDef name e -> mbraces (bindVar name) <+> operator "->" <+> ppExpr e
    
    ppLetBind :: Bind t -> Doc
    ppLetBind (NonRec t obj) = mkBrace
        [ bindVar t <+> equal <+> ppObj obj ]
    ppLetBind (Rec binds) = mkBrace
        [ bindVar x <+> equal <+> ppObj obj
        | (x, obj) <- binds ]
    
    
    ppAtom :: Atom t -> Doc
    ppAtom atom = case atom of
        AVar x -> ppVar x
        ANum n -> num $ integer n
        ADec f -> num $ double f
        AChr c -> chr c
        AUnknown i t -> mbraces $ ppVar (Local i t)

    ppVar :: Var t -> Doc
    ppVar v = case v of
        Heap x    -> var x
        Local n t -> chr '<' <> int n <> comma <> var t <> chr '>'
    
    ppObj :: Obj t -> Doc
    ppObj obj = case obj of
        OFun args size e -> object "FUN" <+> chr '<' <> int size <> chr '>' <+> mparens (hsep (map bindVar args) 
                        <+> operator "->" <+> ppExpr e)
        OPap obj args -> object "PAP" <+> mparens (var obj 
                                                <+> hsep (map ppAtom args))
        OCon name args -> object "CON" <+> mparens (conVar name 
                                                 <+> hsep (map ppAtom args))
        OThunk fv s e -> object "THUNK" <+> mbrackets (hsep $ map ppAtom fv) <+> int s <+> mparens (ppExpr e)
        OOpt   a set -> object "OPT" <+> mparens (ppAtom a) <+> text (show set)
        OBlackhole -> object "BLACKHOLE"

    ppSVal :: SValue t -> Doc
    ppSVal sval = case sval of
        SAtom a -> ppAtom a
        SFun    -> key "<FUN>"
        SCon c sval | null sval -> conVar c
                    | otherwise -> mparens $ conVar c <+> hsep (map ppSVal sval)

    ppAStack :: ArgStack t -> Doc
    ppAStack = vsep . map ppStackFrame

    ppStackFrame :: StackFrame t -> Doc
    ppStackFrame as = list (map ppAtom $ snd as) {-(StackFrame {..}) = (mparens (int lock)) 
        <+> semiBraces [ if p == spointer 
                           then mbrackets (ppAtom a) 
                           else ppAtom a 
                       | (p, a) <- zip [0..] args
                       ] -}

    ppCStack :: ContStack t -> Doc
    ppCStack = vsep . map ppCont


    ppCont :: Cont t -> Doc
    ppCont cont = case cont of
        CtCase brs -> key "case" <+> ppHole <+> key "of"
            <$> indent 4 (mkBrace $ map ppBranch brs)
        CtUpd t -> key "Upd" <+> var t <+> ppHole
        CtArg a -> key "Arg" <+> ppAtom a
        CtOpt t -> key "Opt" <+> var t
        CtPrint -> key "Print"
        CtPrintCon c pr ne -> key "PrintCont" <+> conVar c <+> text (show pr) 
                       <+> mparens (hsep (map ppAtom ne)) 
        CtOFun xs i a -> key "OFun" <+> int i <+> bindVar a 
                       <+> mparens (hsep (map bindVar xs) <+> operator "->" <+> ppHole)
        CtOApp f app  -> key "OApp" <+> ppVar f <+> ppHole <+> hsep (map ppAtom app)
        CtOCase brs -> key "OCase" <+> ppCont (CtCase brs)
        CtOLet x -> key "Olet" <+> bindVar x <+> operator "=" <+> operator "?" <+> key "in" <+> ppHole 
        CtOBranch e brdone brleft ->
            key "OBranch" <+> key "case" <+> ppExpr e <+> key "of"
            <$> indent 4 (mkBrace $ map ppBranch brdone ++ 
                                    case brleft of
                                       BCon c as _:left -> 
                                           conVar c <+> hsep (map bindVar as) <+> operator "->" <+> ppHole :
                                           map ppBranch left
                                       BDef v _:left ->
                                           mbraces (bindVar v) <+> operator "->" <+> ppHole :
                                           map ppBranch left
                                       [] ->
                                           [ppHole])
        CtOUpd t     -> key "OUpd" <+> var t
        CtOInstant n -> key "OInstant" <+> ppAtom (ANum $ toInteger n)
        -- x -> text (show (unsafeCoerce x :: Cont String))

    ppHole = operator "()"
