module Stg.Substitution
  ( subst
  , substList
  , removeOPT
  ) where

import Data.Generics
import Data.Generics.Biplate
import Data.Generics.Uniplate
import Stg.AST


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

substAtom :: Eq t => t -> Atom t -> Atom t -> Atom t
substAtom x x' (AVar v) | {-# SCC "substAtomEq" #-} x == v = x'
substAtom _ _ a = a


substExpr :: Eq t => t -> Atom t -> Expr t -> Expr t
substExpr x (AVar x') (ECall t as) | {-# SCC "substExprAVarEq" #-} x == t = ECall x' as
substExpr x (ANum _)  (ECall t as) | {-# SCC "substExprANumEq" #-} x == t = error "substExpr with ANum" 
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
    f (OOpt x _) = OThunk (EAtom x)
    f x          = x


