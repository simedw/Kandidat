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

import Stg.Heap (Heap,Location(..))
import qualified Stg.Heap as H

isKnown :: Ord t => Heap t -> Atom t -> Bool
isKnown h (AVar t) = maybe False (const True) (H.lookup t h)
isKnown _ _        = True


omega' rule st h e set = returnJust 
    ( rule
    , OmegaState 
        { code     = e
        , stack    = st
        , heap     = h
        , settings = set 
        }
    )

omega :: (Ord t, Data t) => Stack t -> Heap t -> Expr t -> 
                            [StgSettings t] -> StgM t (Maybe (Rule, StgState t))
omega stack heap code set = case code of
    EAtom a@(AVar t) -> -- | isKnown heap a -> 
      case H.locatedLookup t heap of
        Just (OThunk e, OnHeap) -> do
            returnJust
                ( ROpt ORKnownAtom
                , StgState
                    { code  = code
                    , stack = CtUpd t : stack
                    , heap  = heap
                    , settings = set
                    }
                )
        Just (OThunk e, OnAbyss) -> omega' (ROmega "Thunk on abyss") (CtOUpd t : stack) heap e set        
        Just (OCon _ _, _) -> do 
            psi' (ROmega "Atom point at Con") stack heap t set
        _ -> irreducible
    ECase expr brs -> omega' (ROmega "case") (CtOCase brs : stack) heap expr set

    -- Function application with all known arguments,
    -- and with known function.
    ECall f args | isKnown heap (AVar f) && all (isKnown heap) args -> returnJust
        ( ROpt ORKnownCall
        , StgState
            { code  = code
            , stack = stack
            , heap  = heap
            , settings = set
            }
        )

    -- Function application
    ECall f args | canInline f set ->
        case H.locatedLookup f heap of
            -- Known function, inline it!
            Just (OFun as e,OnHeap) | length as == length args -> do
                returnJust
                    ( ROpt ORInline
                    , StgState
                        { code  = EAtom (AVar f)
                        , stack = map CtArg args ++ CtOInstant 1 : stack
                        , heap  = heap
                        , settings = inline f set
                        }
                    )
            -- Unevaluated function, evaluate the thunk!
            Just (OThunk e, OnHeap) -> 
                returnJust
                    ( ROpt ORAppThunk
                    , StgState
                        { code     = e
                        , stack    = CtUpd f : CtOApp args : stack
                        , heap     = heap
                        , settings = set
                        }
                    )
            -- Unevaluated, abyssimal function, omega the thunk!
            Just (OThunk e, OnAbyss) -> 
                omega' (ROmega "function abyss thunk") (CtOApp args : stack) heap e set
            -- PAP cases??
            _ -> irreducible
    ELet (NonRec x o) e' -> do
        x' <- newVar
        omega' (ROmega "let") (CtOLet x' : stack) (H.insertAbyss x' o heap) (subst x (AVar x') e') set
    
    c@(EPop _ as) | all (isKnown heap) as -> returnJust 
        ( ROpt ORPOp
        , StgState
            { code     = c
            , stack    = CtOInstant 2 : stack -- 1?
            , heap     = heap
            , settings = set
            }
        )
    
    {-
    ELet (NonRec x (OThunk e))  e' -> do
        x' <- newVar
        omega' ROmega (CtOLetThunk x' (subst x (AVar x') e') : stack) heap e set
    ELet (NonRec x (OCon c as)) e' 
        | all (isKnown heap) as -> do
            x' <- newVar
            omega' ROmega stack (M.insert x' (OCon c as) heap) (subst x (AVar x') e') set
        | otherwise -> omega' ROmega (CtOLetObj x (OCon c as):stack) heap e' set
    -}
    _ -> irreducible
  where
    irreducible = irr' (ROmega "irreducible from omega") stack heap code set

beta :: (Ord t, Data t) => Stack t -> Heap t -> 
                           [StgSettings t] -> StgM t (Maybe (Rule, StgState t))
beta stack@(CtOBranch e brdone brleft:ss) h set = case brleft of
    BDef x e   :_ -> omega stack h e set
    BCon c as e:_ -> omega stack h e set
    []            -> irr' (RIrr "case expression finished (from beta)") ss h (ECase e brdone) set

irr' rule st h e set = returnJust 
    ( rule
    , IrrState 
        { code     = e
        , stack    = st
        , heap     = h
        , settings = set 
        }
    )

irr :: (Ord t, Data t) => Stack t -> Heap t -> Expr t -> 
                          [StgSettings t] -> StgM t (Maybe (Rule, StgState t))
irr (CtOCase brs     : ss) h e  set = 
    if caseBranches (head set) 
       then beta (CtOBranch e [] brs:ss) h set
       else irr' (RIrr "case continuation") ss h (ECase e brs) set
irr (CtOLet t        : ss) h e set = case H.lookupAbyss t h of
    Just o  -> irr' (RIrr "let continuation") ss h (ELet (NonRec t o) e) set
    Nothing -> error "irr on CtOLet, variable not in abyss!"
    
irr (CtOFun xs a     : ss) h e  set = do
    let h' = H.insert a (OFun xs e) h 
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
    irr' (RIrr "app continuation") ss h (ELet (NonRec v (OThunk e')) (ECall v as)) set
irr (CtOUpd v : ss) h e set = do
    let h' = H.insertAbyss v (OThunk e) h
    irr' (RIrr "update (abyss) thunk") ss h' e set

psi' rule st h v set = returnJust 
    ( rule
    , PsiState 
        { code     = EAtom (AVar v)
        , stack    = st
        , heap     = h
        , settings = set 
        }
    )

psi :: (Ord t, Data t) => Stack t -> Heap t -> t -> 
                          [StgSettings t] -> StgM t (Maybe (Rule, StgState t)) 

-- Brave assumption: The let is dead code
--psi (CtOLet t : ss) h v set = psi' (RPsi "Let continuation") ss h v set
psi ss@(CtOLet t : _) h v set = do
    let (lets, rest) = break p ss
    case rest of
        CtOCase brs : rest -> psi' (RPsi "Dans fulhack med let,case elim") (CtOCase brs : lets ++ rest) h v set
        _ -> irr' (RPsi "ingen case efter lets i Dans fulhack") ss h (EAtom (AVar v)) set
  where 
    p (CtOLet _) = False
    p _          = True


psi (CtOCase branch   : ss) h v set = case H.lookupAnywhere v h of
    Just (OCon c atoms) -> 
        case instantiateBranch c atoms branch of 
            Nothing -> def
            Just expr -> om expr
    Just o -> def
    Nothing -> err
  where
    err = error "psi couldn't instantiate that :'/"
    om e = omega' (RPsi "KnownCase") ss h e set
    def = case findDefaultBranch (AVar v) branch of
        Nothing -> err
        Just expr -> om expr
psi (CtOUpd t   : ss) h v set = case H.lookupAnywhere v h of
    Just o -> let h' = H.insertAbyss t o h
               in psi' (RPsi "OUpd thunk") ss h' v set
    Nothing -> error $ "psi didn't find that OUpd "
psi (CtUpd t   : ss) h v set = case H.lookupHeap v h of
    Just o -> let h' = H.insertAbyss t o h
               in psi' (RPsi "OUpd thunk") ss h' v set
    _ -> error $ "psi CtUpd, is it on the abyss? or not at all???? :O"
    
               
psi ss@(CtOBranch e brdone brleft : _) h v set = 
   irr' (RPsi "branch continuation") ss h (EAtom (AVar v)) set
psi ss@(CtOFun args alpha : _) h v         set =
    irr' (RPsi "fun continuation") ss h (EAtom (AVar v)) set
psi (CtOApp as : ss) h v set = omega' (RPsi "App continutation") ss h (ECall v as) set

psi s h v _ = error $ "Psi: I don't know what to do with this stack: " 
     ++ show (unsafeCoerce s :: Stack String)
