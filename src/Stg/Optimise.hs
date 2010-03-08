module Stg.Optimise where

import Control.Monad

import Unsafe.Coerce

import Data.Generics
import Data.Generics.PlateData

import Data.Map(Map)
import qualified Data.Map as M

import Stg.AST
import Stg.Types
import Stg.Rules
import Stg.Branch
import Stg.Substitution

isKnown :: Ord t => Heap t -> Atom t -> Bool
isKnown h (AVar t) = maybe False (const True) (M.lookup t h)
isKnown _ _        = True

--isBlue :: Ord t => Heap t -> Expr t -> Bool
--isBlue h ex = case ex of
    

omega :: (Ord t, Data t) => Stack t -> Heap t -> Expr t-> StgM t (Maybe (Rule, StgState t))
omega stack heap code = case code of
    EAtom a@(AVar t) | isKnown heap a -> case M.lookup t heap of
        Just (OThunk e) -> returnJust
            ( ROpt ORKnownAtom
            , StgState
                { code  = code
                , stack = stack
                , heap  = heap
                }
            )
        _ -> irreducible
    ECase expr brs -> omega (CtOCase brs : stack) heap expr
    ECall f args | isKnown heap (AVar f) && all (isKnown heap) args -> returnJust
        ( ROpt ORKnownCall
        , StgState
            { code  = code
            , stack = stack
            , heap  = heap
            }
        )

    ECall f args | isKnown heap (AVar f) && any (isKnown heap) args ->
        case M.lookup f heap of
            Just (OFun as e) | length as == length args ->
                returnJust
                    ( ROpt ORInline
                    , StgState
                        { code  = EAtom (AVar f)
                        , stack = map CtArg args ++ CtOInstant 1 : stack
                        , heap  = heap
                        }
                    )
            _ -> irreducible
    ELet (NonRec x (OThunk e))  e' -> do
        x' <- newVar
        omega (CtOLetThunk x' (subst x (AVar x') e') : stack) heap e
    ELet (NonRec x (OCon c as)) e' 
        | all (isKnown heap) as -> do
            x' <- newVar
            omega stack (M.insert x' (OCon c as) heap) (subst x (AVar x') e')
        | otherwise -> omega (CtOLetObj x (OCon c as):stack) heap e'           

    _ -> irreducible
  where
    irreducible = irr stack heap code

beta :: (Ord t, Data t) => Stack t -> Heap t -> StgM t (Maybe (Rule, StgState t))
beta stack@(CtOBranch e brdone brleft:ss) h = case brleft of
    BDef x e   :_ -> omega stack h e
    BCon c as e:_ -> omega stack h e
    []            -> irr ss h (ECase e brdone)


irr :: (Ord t, Data t) => Stack t -> Heap t -> Expr t -> StgM t (Maybe (Rule, StgState t))
irr (CtOLetThunk x e : ss) h e' = omega (CtOLetObj x (OThunk e') : ss) h e
-- irr (CtOCase brs     : ss) h e  = irr ss h (ECase e brs)
irr (CtOCase brs     : ss) h e  = beta (CtOBranch e [] brs:ss) h 
irr (CtOLetObj x o   : ss) h e  = irr ss h (ELet (NonRec x o) e)
irr (CtOFun xs a     : ss) h e  = do 
    let h' = M.insert a (OFun xs e) h 
    returnJust
         ( ROpt ORDone
         , StgState
            { code  = EAtom (AVar a) 
            , stack = ss
            , heap  = h'
            }
         )
irr (CtOBranch e brdone (BDef x _   :brleft) : ss) h e' = beta (CtOBranch e (brdone ++ [BDef x    e']) brleft:ss) h
irr (CtOBranch e brdone (BCon c as _:brleft) : ss) h e' = beta (CtOBranch e (brdone ++ [BCon c as e']) brleft:ss) h

psi :: (Ord t, Data t) => Stack t -> Heap t -> t -> StgM t (Maybe (Rule, StgState t))
psi (CtOLetThunk t e : ss) h v = omega ss h (subst t (AVar v) e)
psi (CtOLetObj t obj : ss) h v = irr ss h (ELet (NonRec t obj) (EAtom (AVar v)))
psi (CtOCase brs     : ss) h v = returnJust $ 
    ( ROpt ORKnownCase
    , StgState
        { code  = EAtom (AVar v)
        , stack = CtCase brs : CtOInstant 2 : ss
        , heap  = h
        }
    )
psi ss@(CtOBranch e brdone brleft : _) h v = irr ss h (EAtom (AVar v))
psi ss@(CtOFun args alpha : _) h v = irr ss h (EAtom (AVar v))  
psi s h v = error $ show (unsafeCoerce s :: Stack String)
