{-# LANGUAGE PackageImports #-}
module Stg.Substitution
  ( subst
  , substList
  , removeOPT
  , subtractLocal
  , inltP
  , inltM
  , inlt -- :( this special one needs in case branches
  , shift
  , localsE  
  ) where

import "syb" Data.Generics
import Data.Generics.Biplate
import Data.Generics.Uniplate
import Stg.AST
import Stg.Variable

import qualified Data.Map as M

{-
var = EAtom . AVar

test :: Expr String
test = ECase (var "xs")
             [BCon "Cons" ["y","ys"]
                (ELet (Rec [("h",OThunk (ECall "f" [AVar "y"]))
                           ,("t",OThunk (ECall "map" [AVar "f", AVar "ys"]))
                           ,("x",OOpt (AVar "function") [])
                           ,("r",OCon "Cons" [AVar "h", AVar "t"])
                           ]) (var "r"))
             , BDef "x" (var "nil")
             ]
-}

substAtom :: Eq t => t -> Atom t -> Atom t -> Atom t
substAtom x x' (AVar (Heap v)) | {-# SCC "substAtomEq" #-} x == v = x'
substAtom x x' (AVar (Local _ v)) | x == v = x'
substAtom _ _ a = a


substExpr :: Eq t => t -> Atom t -> Expr t -> Expr t
substExpr x (AVar x') (ECall (Heap t) as) | {-# SCC "substExprAVarEq" #-} x == t = ECall x' as
substExpr x (ANum _)  (ECall (Heap t) as) | {-# SCC "substExprANumEq" #-} x == t = error "substExpr with ANum" 
substExpr _ _ e = e

subst :: (Data t, Eq t) => t -> Atom t -> Expr t -> Expr t
subst x x' = transformBi (substExpr x x') . transformBi (substAtom x x') 

substList :: (Data t, Eq t) => [t] -> [Atom t] -> Expr t -> Expr t
substList []     []     = id
substList (x:xs) (y:ys) = substList xs ys . subst x y

   
-- I can't get this to work with just t

removeOPT :: Function String -> Function String 
removeOPT = transformBi f
  where
    f :: Obj String -> Obj String
    f (OOpt x _) = case x of
        AVar (Local _ _) -> OThunk [x] 1 (EAtom (AVar (Local 0 "hej")))
        _                -> OThunk [] 0 (EAtom x)
    f x          = x


subtractLocal :: Variable t => t -> Int -> Expr t -> Expr t
subtractLocal v n = transformBi (subtractLocalAtom v n) -- v not used :)

subtractLocalAtom :: Variable t => t -> Int -> Atom t -> Atom t
subtractLocalAtom _ n a = case a of
    AVar (Local i v) -> AVar (Local (i - n) v)
    _                -> a

inltM :: Variable t => [Atom t] -> Int -> Expr t -> Expr t
inltM as sp = inlt (>= sp) 0 sp as

inltP :: Variable t => [Atom t] -> Int -> Expr t -> Expr t
inltP as sp = inlt (const True) sp 0 as

{-
inlt g s st as = transformBi (ct aux) . transformBi (ct' aux')
  where
    aux i t | g i = case M.lookup i m of
        Nothing -> AVar $ Local (s + i - length as) t
        Just a  -> a
    aux i t = AVar (Local i t)
    aux' i t | g i = case M.lookup i m of
        Just (AVar v) -> v
        _             -> Local (s + i - length as) t
    aux' i t = Local i t
    m       = M.fromList $ zip [st..] as
-}


shift :: Variable t => (Int -> Bool) -> Int -> Expr t -> Expr t
shift p d = inlt p d 0 []

inlt :: Variable t 
     => (Int -> Bool) -- ^ guard
     -> Int           -- ^ add
     -> Int           -- ^ start (atoms)
     -> [Atom t]      -- ^ subst atoms
     -> Expr t -> Expr t
inlt g s st as = inlE atom var
  where
    atom a = case a of
        AVar (Local i t) | g i -> case M.lookup i m of
            Just a  -> a
            Nothing -> AVar $ Local (i + s - length as) t
        _ -> a
    var v  = case v of
        Local i t | g i -> case M.lookup i m of
            Just (AVar v) -> v
            Nothing       -> Local (i + s - length as) t
        _ -> v
    m         = M.fromList $ zip [st..] as

inlE :: Variable t => (Atom t -> Atom t) -> (Var t -> Var t) -> Expr t -> Expr t
inlE fa fv e = case e of
    EAtom a    -> EAtom (fa a)
    ECall v as -> ECall (fv v) (map fa as)
    EPop op as -> EPop op (map fa as)
    ELet b e   -> ELet (inlBind fa fv b) (inlE fa fv e)
    ECase e bs -> ECase (inlE fa fv e) (map (inlBr fa fv) bs)
    _          -> e

inlO :: Variable t => (Atom t -> Atom t) -> (Var t -> Var t) -> Obj t -> Obj t
inlO fa fv o = case o of
    OPap v as     -> OPap v (map fa as)
    OCon c as     -> OCon c (map fa as)
    OThunk as i e -> OThunk (map fa as) i e
    OOpt a sets   -> OOpt (fa a) sets
    _             -> o

inlBind :: Variable t => (Atom t -> Atom t) -> (Var t -> Var t) -> Bind t -> Bind t
inlBind fa fv b = case b of
    NonRec v o -> NonRec v (inlO fa fv o)
    Rec vs     -> Rec  [(v, inlO fa fv o) | (v, o) <- vs]

inlBr :: Variable t => (Atom t -> Atom t) -> (Var t -> Var t) -> Branch t -> Branch t
inlBr fa fv b = case b of
    BCon v vs e -> BCon v vs (inlE fa fv e)
    BDef v    e -> BDef v    (inlE fa fv e)
    
    
    
    
    
localsVar :: Variable t => Var t -> [Var t]
localsVar v = case v of
    Local _ _ -> [v]
    _         -> []

localsAtom :: Variable t => Atom t -> [Var t]
localsAtom a = case a of
    AVar l -> localsVar l
    _      -> []
   
localsE :: Variable t => Expr t -> [Var t]
localsE e = case e of
    EAtom a    -> localsAtom a
    ECall v as -> localsVar v ++ concatMap localsAtom as
    EPop op as -> concatMap localsAtom as
    ELet b e   -> localsBind b ++ localsE e
    ECase e bs -> localsE e ++ concatMap localsBr bs
    _          -> []

localsO :: Variable t => Obj t -> [Var t]
localsO o = case o of
    OPap v as     -> concatMap localsAtom as
    OCon c as     -> concatMap localsAtom as
    OThunk as i e -> concatMap localsAtom as
    OOpt a sets   -> localsAtom a
    _             -> []

localsBind :: Variable t => Bind t -> [Var t]
localsBind b = case b of
    NonRec v obj -> localsO obj
    Rec xs       -> concatMap (localsO . snd) xs

localsBr :: Variable t => Branch t -> [Var t]
localsBr b = case b of
    BCon v vs e -> localsE e
    BDef v    e -> localsE e