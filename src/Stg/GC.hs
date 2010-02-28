{-# LANGUAGE ScopedTypeVariables #-}
module Stg.GC where

-- Bindings
-- Let, Fun, Case branches 

-- The plan

-- Traverse the tree, monadically, and add all bindings
-- Extract all variables
-- Set difference of these is the free variables

import Unsafe.Coerce

import Stg.AST
import Stg.Types

import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map(Map)
import qualified Data.Map as M


var = EAtom . AVar

test :: Expr String
test = ECase (var "xs")
             [BCon "Cons" ["y","ys"]
                (ELet False [("h",OThunk (ECall "f" [AVar "y"]))
                            ,("t",OThunk (ECall "map" [AVar "f", AVar "ys"]))
                            ,("r",OCon "Cons" [AVar "h", AVar "t"])
                            ] (var "r"))
             , BDef "x" (var "nil")
             ]

mkGC :: forall t . Ord t => [t] -> StgState t -> StgState t
mkGC untouchable (StgState code stack heap) = 
    let initial = freeVars code `S.union` freeVarsList stack
    in  StgState code stack $ heapify $ gcStep (initial`S.union` S.fromList untouchable) initial
  where
    gcStep :: Set t -> Set t -> Set t
    gcStep acc s | S.null s  = acc
                 | otherwise = 
                    let acc' = ( S.unions 
                              $ map (\x -> freeVars 
                                    $ fel x 
                                    $ M.lookup x heap) 
                              $ S.toList s
                              ) 
                    in  gcStep (acc' `S.union` acc) (acc' `S.difference` acc)

    heapify :: Set t -> Heap t
    heapify s = M.filterWithKey (\k a -> S.member k s) heap

    fel :: t -> Maybe (Obj t) -> Obj t
    fel x t = maybe (error $ "GC: couldn't find: " ++ unsafeCoerce x) id t

class FV e where
    freeVars :: Ord t => e t -> Set t 

instance FV Expr where
    freeVars (EAtom e)       = freeVars e
    freeVars (ECall i as)    = S.insert i (freeVarsList as)
    freeVars (EPop  _ as)    = freeVarsList as
    freeVars (ELet _ defs e) = 
        let (range,domain)   = unzip defs
        in  (freeVarsList domain `S.union` freeVars e) `S.difference` S.fromList range
    freeVars (ECase e brs)   = freeVars e `S.union` freeVarsList brs

instance FV Atom where
    freeVars (AVar v)        = S.singleton v
    freeVars _               = S.empty

freeVarsList :: (FV a, Ord t) => [a t] -> Set t
freeVarsList = S.unions . map freeVars

instance FV Branch where
    freeVars (BCon c as e)   = freeVars e `S.difference` S.fromList as
    freeVars (BDef x e)      = S.delete x (freeVars e) 

instance FV Obj where
    freeVars (OFun as e)     = freeVars e `S.difference` S.fromList as
    freeVars (OPap f as)     = S.insert f (freeVarsList as)
    freeVars (OCon c as)     = freeVarsList as
    freeVars (OThunk e)      = freeVars e
    freeVars (OBlackhole)    = S.empty
    freeVars (OOpt a)        = freeVars a

instance FV Cont where
    freeVars (CtCase brs)    = freeVarsList brs
    freeVars (CtUpd i)       = S.singleton i
    freeVars (CtArg a)       = freeVars a
    freeVars (CtOpt i)       = S.singleton i
    freeVars (CtContOpt i)   = S.singleton i
    
