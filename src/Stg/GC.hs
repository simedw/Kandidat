{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Stg.GC where

-- Bindings
-- Let, Fun, Case branches 

-- The plan

-- Traverse the tree, monadically, and add all bindings
-- Extract all variables
-- Set difference of these is the free variables

import Unsafe.Coerce
import Debug.Trace

import Stg.AST
import Stg.Types
import Stg.Stack
import Stg.Variable

import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map as M

import Stg.Heap (Heap)
import qualified Stg.Heap as H

{-
var = EAtom . AVar . Heap

test :: Expr String
test = ECase (var "xs")
             [BCon "Cons" ["y","ys"]
                (ELet (Rec [("h",OThunk (ECall "f" [AVar "y"]))
                           ,("t",OThunk (ECall "map" [AVar "f", AVar "ys"]))
                           ,("r",OCon "Cons" [AVar "h", AVar "t"])
                           ]) (var "r"))
             , BDef "x" (var "nil")
             ]
-}

mkGC :: forall t . Variable t => [t] -> StgState t -> StgState t
mkGC untouchable st = 
    let initial = freeVars (code st) `S.union` freeVarsList (cstack st) `S.union` freeVarsList (concat $ astack st)
    in  st{ heap = heapify $ gcStep (initial`S.union` S.fromList untouchable) initial}
  where
    gcStep :: Set t -> Set t -> Set t
    gcStep acc s | S.null s  = acc
                 | otherwise = 
                    let acc' = ( S.unions 
                              $ map (\x -> freeVars 
                                    $ fel x 
                                    $ H.lookupAnywhere x (heap st)) 
                              $ S.toList s
                              ) 
                    in  gcStep (acc' `S.union` acc) (acc' `S.difference` acc)

    heapify :: Set t -> Heap t
    heapify s = M.filterWithKey (\k a -> S.member k s) (heap st)

    fel :: t -> Maybe (Obj t) -> Obj t
    fel x t = maybe (trace ("GC: couldn't find: " ++ unsafeCoerce x) OBlackhole) id t

class FV e where
    freeVars :: Ord t => e t -> Set t 

instance FV Expr where
    freeVars (EAtom e)       = freeVars e
    freeVars (ECall i as)    = freeVars i `S.union` (freeVarsList as)
    freeVars (EPop  _ as)    = freeVarsList as
    freeVars (ELet binds e)  = freeVars binds `S.union` 
      (freeVars e `S.difference` S.fromList (map fst (getBinds binds)))
    freeVars (ECase e brs)   = freeVars e `S.union` freeVarsList brs
    freeVars (ESVal _)       = S.empty

instance FV Bind where
    freeVars (NonRec t obj) = freeVars obj
    freeVars (Rec binds)    = 
        let (range, domain)  = unzip binds
        in freeVarsList domain `S.difference` S.fromList range

instance FV Atom where
    freeVars (AVar v)        = freeVars v
    freeVars _               = S.empty

instance FV Var where -- we are a bit unsure here...
    freeVars (Heap v)    = S.singleton v
    freeVars (Local _ v) = S.empty          
      
freeVarsList :: (FV a, Ord t) => [a t] -> Set t
freeVarsList = S.unions . map freeVars

instance FV Branch where
    freeVars (BCon c as e)   = freeVars e `S.difference` S.fromList as
    freeVars (BDef x e)      = S.delete x (freeVars e) 

instance FV Obj where
    freeVars (OFun as _ e)   = freeVars e `S.difference` S.fromList as
    freeVars (OPap f as)     = S.insert f (freeVarsList as)
    freeVars (OCon c as)     = freeVarsList as
    freeVars (OThunk fv _ e) = freeVarsList fv `S.union` freeVars e
    freeVars (OBlackhole)    = S.empty
    freeVars (OOpt a set)    = freeVars a `S.union` freeVarsList set 

instance FV Setting where
    freeVars (Inlinings a)   = freeVars a
    freeVars (Inline t a)    = freeVars a
    freeVars CaseBranches    = S.empty

instance FV Cont where
    freeVars (CtCase brs)           = freeVarsList brs
    freeVars (CtUpd i)              = S.singleton i
    freeVars (CtOUpd i)             = S.singleton i
    freeVars (CtArg a)              = freeVars a
    freeVars (CtOpt i)              = S.singleton i
    freeVars (CtPrint)              = S.empty
    freeVars (CtPrintCon _ _ as)    = freeVarsList as
    freeVars (CtOFun args alpha)    = S.singleton alpha
    freeVars (CtOCase brs)          = freeVarsList brs
    freeVars (CtOBranch e brs brs') = freeVars e `S.union` freeVarsList brs `S.union` freeVarsList brs'
    freeVars (CtOLet v)             = S.singleton v 
    freeVars (CtOInstant _)         = S.empty
    freeVars (CtOApp as)            = freeVarsList as
{-    
instance FV StackFrame where
    freeVars (StackFrame _ p args)  = freeVarsList (take p args) -- ... bad ...
-}