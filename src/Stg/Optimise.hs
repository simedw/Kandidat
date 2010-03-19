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
    
outputVar :: String -> t -> StgM t ()
outputVar str t = output $ str ++ ": " ++ unsafeCoerce t

omega :: (Ord t, Data t) => Stack t -> Heap t -> Expr t -> 
                            [StgSettings t] -> StgM t (Maybe (Rule, StgState t))
omega stack heap code set = case code of
    EAtom a@(AVar t) | isKnown heap a -> 
      case M.lookup t heap of
        Just (OThunk e) -> do
            outputVar "omega: EAtom, OThunk" t
            returnJust
                ( ROpt ORKnownAtom
                , StgState
                    { code  = code
                    , stack = stack
                    , heap  = heap
                    , settings = set
                    }
                )        
        Just (OCon _ _) -> do 
            outputVar "omega: EAtom , OCon" t 
            psi stack heap t set
        _ -> irreducible
    ECase expr brs -> output "omega: ECase" >> omega (CtOCase brs : stack) heap expr set
    ECall f args | isKnown heap (AVar f) && all (isKnown heap) args -> returnJust
        ( ROpt ORKnownCall
        , StgState
            { code  = code
            , stack = stack
            , heap  = heap
            , settings = set
            }
        )

    ECall f args | isKnown heap (AVar f) 
                 -- && any (isKnown heap) args 
                 && canInline f set ->
        case M.lookup f heap of
            Just (OFun as e) | length as == length args -> do
                output "omega ecall, inline"
                returnJust
                    ( ROpt ORInline
                    , StgState
                        { code  = EAtom (AVar f)
                        , stack = map CtArg args ++ CtOInstant 1 : stack
                        , heap  = heap
                        , settings = inline f set
                        }
                    )
            Just (OThunk a) -> do
                output "omega: ecall, thunk as function" 
                omega (CtOApp args : stack) heap a set
            {- returnJust
                ( ROpt ORInline
                , StgState
                    { code  = a
                    , stack = CtOApp args  : stack
                    , heap  = heap
                    , settings = set
                    }
                )
              -}
            _ -> irreducible
    ELet (NonRec x (OThunk e))  e' -> do
        output "omega, allocate dangerous thunk"
        x' <- newVar
        omega (CtOLetThunk x' (subst x (AVar x') e') : stack) heap e set
    ELet (NonRec x (OCon c as)) e' 
        | all (isKnown heap) as -> do
            output "omega, allocate Con"
            x' <- newVar
            omega stack (M.insert x' (OCon c as) heap) (subst x (AVar x') e') set
        | otherwise -> do
            output "omega, tried allocate Con, failed, trying else"
            omega (CtOLetObj x (OCon c as):stack) heap e' set

    _ -> irreducible
  where
    irreducible = irr stack heap code set

beta :: (Ord t, Data t) => Stack t -> Heap t -> 
                           [StgSettings t] -> StgM t (Maybe (Rule, StgState t))
beta stack@(CtOBranch e brdone brleft:ss) h set = case brleft of
    BDef x e   :_ -> outputVar "beta, def" x >> omega stack h e set
    BCon c as e:_ -> outputVar "beta, con" c >>omega stack h e set
    []            -> irr ss h (ECase e brdone) set


irr :: (Ord t, Data t) => Stack t -> Heap t -> Expr t -> 
                          [StgSettings t] -> StgM t (Maybe (Rule, StgState t))
irr (CtOLetThunk x e : ss) h e' set = do
    output "irr translate thunk to obj"
    omega (CtOLetObj x (OThunk e') : ss) h e set
irr (CtOCase brs     : ss) h e  set = do
    if caseBranches (head set) 
       then beta (CtOBranch e [] brs:ss) h set
       else irr ss h (ECase e brs) set
irr (CtOLetObj x o   : ss) h e  set = irr ss h (ELet (NonRec x o) e) set 
irr (CtOFun xs a     : ss) h e  set = do
    let h' = M.insert a (OFun xs e) h 
    returnJust
         ( ROpt ORDone
         , StgState
            { code  = EAtom (AVar a) 
            , stack = ss
            , heap  = h'
            , settings = set
            }
         )
irr (CtOBranch e brdone (BDef x _   :brleft) : ss) h e' set = 
        beta (CtOBranch e (brdone ++ [BDef x    e']) brleft:ss) h set
irr (CtOBranch e brdone (BCon c as _:brleft) : ss) h e' set =
        beta (CtOBranch e (brdone ++ [BCon c as e']) brleft:ss) h set
irr (CtOApp as : ss) h e' set = do
    v <- newVar
    outputVar "irr, CtOApp" v
    irr ss h (ELet (NonRec v (OThunk e')) (ECall v as)) set

psi :: (Ord t, Data t) => Stack t -> Heap t -> t -> 
                          [StgSettings t] -> StgM t (Maybe (Rule, StgState t)) 
psi (CtOLetThunk t e : ss) h v set =  
      outputVar "psi, CtOLetThunk" v
   >> omega ss h (subst t (AVar v) e) set 
psi (CtOLetObj t obj : ss) h v set =  
      outputVar "psi, CtOLetObj" v
   >> irr ss h (ELet (NonRec t obj) (EAtom (AVar v))) set
psi (CtOCase brs     : ss) h v set = returnJust $ 
    ( ROpt ORKnownCase
    , StgState
        { code  = EAtom (AVar v)
        , stack = CtCase brs : CtOInstant 2 : ss
        , heap  = h
        , settings = set
        }
    )
psi ss@(CtOBranch e brdone brleft : _) h v set = 
      outputVar "psi, CtOBranch" v
   >> irr ss h (EAtom (AVar v)) set
psi ss@(CtOFun args alpha : _) h v         set =
      outputVar "psi, CtOFun" v
   >> irr ss h (EAtom (AVar v)) set
psi (CtOApp as : ss) h v set =  outputVar "psi, CtoApp" v 
                             >> omega ss h (ECall v as) set

psi s h v _ = error $ show (unsafeCoerce s :: Stack String)
