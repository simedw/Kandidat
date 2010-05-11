{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}
module Stg.Optimise where

import Control.Monad
import Control.Applicative
import Control.Arrow

import "syb" Data.Generics
import Data.Generics.PlateData
import qualified Data.Set as S
import Data.Map(Map)
import qualified Data.Map as M
import Data.List

import Stg.AST
import Stg.Branch
import Stg.GC
import Stg.Heap (Heap,Location(..))
import qualified Stg.Heap as H
import Stg.Rules
import Stg.Substitution
import Stg.Stack
import Stg.Types
import Stg.Variable

isKnownStack :: Ord t => ArgStack t -> Heap t -> Atom t -> Bool
isKnownStack s h a = case lookupAtomStack s a of
    AVar (Heap t) -> maybe False (const True) (H.lookup t h)
    AUnknown _ _  -> False
    _             -> True

lookupAtomStack astack (AVar v) = case v of
    Heap x    -> AVar v
    Local x _ -> lookupStackFrame x astack
lookupAtomStack astack    a     = a

-- | Translate Unknowns to locals
trUnknown :: Obj t -> Obj t
trUnknown o = case o of
    OCon c atoms    -> OCon c (map loc atoms)
    OThunk fv s exp -> OThunk (map loc fv) s exp
    OPap   t atoms  -> OPap t (map loc atoms)
    OOpt alpha set  -> OOpt (loc alpha) set
    _               -> o
  where
    loc a = case a of
        AUnknown i t -> AVar $ Local i t
        x            -> x

knownObj :: Variable t => ArgStack t -> Heap t -> Obj t -> Bool
knownObj astack heap obj = case obj of
    OCon c atoms -> all (isKnownStack astack heap) atoms
    OPap f atoms -> all (isKnownStack astack heap) atoms
    OThunk as _ _ -> all (isKnownStack astack heap) as
    _ -> False


-- | Lookup atoms if so no locals are on heap
allocObjStack :: ArgStack t -> Obj t -> Obj t
allocObjStack astack o = case o of
    OCon c atoms    -> OCon c (map lookupAtom atoms)
    OThunk fv s exp -> OThunk (map lookupAtom fv) s exp
    OPap   t atoms  -> OPap t (map lookupAtom atoms)
    OOpt alpha set  -> OOpt (lookupAtom alpha) set
    _               -> o
  where
    lookupAtom = lookupAtomStack astack


omega rule cst ast h e set = returnJust 
    ( rule
    , OmegaState 
        { code     = e
        , cstack   = cst
        , astack   = ast
        , heap     = h
        , settings = set 
        }
    )

omega' :: Variable t => ContStack t -> ArgStack t -> Heap t -> Expr t 
                    -> [StgSettings t] -> StgM t (Maybe (Rule, StgState t))
omega' stack astack heap code set = case code of
    EAtom a -> case lookupAtom a of
        AVar (Heap t) -> case H.locatedLookup t heap of
            {-
                Found a thunk on heap, evaluate on machine
            -}
            Just (OThunk _fv _size _e, OnHeap) -> do
                returnJust
                    ( ROpt ORKnownAtom
                    , StgState
                        { code     = EAtom a
                        , cstack   = stack
                        , astack   = duplicateFrame astack
                        , heap     = heap
                        , settings = set
                        }
                    )
            -- Thunk on abyss, inline
            -- abyss till BLACKHOLE
            -- iaf, sa att den dodkodseliminerar
            -- forstor alla chanser till sharing och ett vardigt bemotande
            -- ocksa alla chanser att fa en CtOUpd
            {-
                A thunk found on abyss, we inline it in the same stackframe
                ! This could be dangerous if they refer to another stackframe
                At the moment we don't know if it is safe
                ! We lose sharing since we can't update this
            -}
            Just (OThunk fv _size e, OnAbyss) -> omega
                (ROmega "Thunk on abyss") stack -- (CtOUpd t : stack) 
                astack heap (inltP fv (getCurrentSP astack) e) set        
            {-
            -}
            _ -> psi (ROmega "Atom don't point at thunk, assume value") 
                      stack astack heap a [] set
        AUnknown _ _ -> irreducible
        _ -> psi (ROmega "Atom is a value") 
                      stack astack heap a [] set

    ECase (ECase expr ibrs) obrs | length ibrs <= 100 -> do
        let e' = ECase expr [ reshuffle br obrs | br <- ibrs]
            reshuffle (BDef x e)
            	= BDef x . ECase e . map (transformBranch 1)
            reshuffle (BCon c args e)
                = BCon c args . ECase e . map (transformBranch $ length args)
            transformBranch = brt . shift (>= getCurrentSP astack)
              where
                brt f (BDef x e)      = BDef x $ f e
                brt f (BCon c args e) = BCon c args $ f e
        omega (ROmega "well known case-law") stack astack heap e' set 

    ECase expr brs -> omega (ROmega "case") (CtOCase brs : stack) 
                            (duplicateFrame astack) heap expr set

    -- Function application with all known arguments,
    -- and with known function.
    ECall f args | isKnown heap (AVar f) && all (isKnown heap) args -> returnJust
        ( ROpt ORKnownCall
        , StgState
            { code  = code
            , cstack = stack
            , astack = duplicateFrame astack
            , heap  = heap
            , settings = set
            }
        )

    -- Function application
    -- What has happened here?! This is an overlapped branch says GHC
    ECall (lookupAtom . AVar &&& id -> (AVar (Heap f'), v)) args | canInline f' set -> let f = Heap f' in
        case H.locatedLookup f' heap of
            -- Known function, inline it!
            Just (OFun as _i e,OnHeap) | length as == length args ->
                  omega (ROpt ORInline) stack astack heap
                        (inltP args (getCurrentSP astack) e)
                        (inline f' set)
            Just (OPap fun as, OnHeap) ->
                omega (ROmega "apply a pap") stack astack heap (ECall (Heap fun) (as ++ args)) set
{-                returnJust
                    ( ROpt ORInline
                    , StgState
                        { code  = inltP (map lookupAtom args)  (getCurrentSP astack) e--EAtom (AVar f)
                        , cstack = stack
                        , astack = astack
                        , heap  = heap
                        , settings = inline f' set
                        }
                    )
-}
            -- Unevaluated function, evaluate the thunk!
            Just (OThunk _as _i e, OnHeap) -> 
                returnJust
                    ( ROpt ORAppThunk
                    , StgState
                        { code     = EAtom (AVar f)
                        , cstack   = CtOApp v args : stack
                        -- This makes sense! one duplicate for machine, one because we
                        -- evaluate the function. CtOApp is like a case in that regard
                        , astack   = duplicateFrame $ duplicateFrame astack -- <-
                        , heap     = heap
                        , settings = set
                        }
                    )
            -- Unevaluated, abyssimal function, omega the thunk!
            Just (OThunk as _i e, OnAbyss) -> 
                omega (ROmega "function abyss thunk") (CtOApp v args : stack) 
                      (duplicateFrame astack) heap (EAtom $ AVar f) set -- hellre lattare regler
                      -- astack heap (inltP as (getCurrentSP astack) e) set
            
            -- PAP cases??
            _ -> irreducible
    -- Can check other obj aswell :)
    ELet (NonRec x o) e' | knownObj astack heap o -> do
        x' <- newVar
        omega (ROmega "let known con") (CtOLet x' : stack) 
              (pushArgs [AVar $ Heap x'] astack) (H.insert x' (allocObjStack astack o) heap) e' set
    ELet (NonRec x o) e' -> do
        x' <- newVar
        omega (ROmega "let, allocate on abyss") 
              (CtOLet x' : stack) 
              (pushArgs [AVar $ Heap x'] astack) 
              (H.insertAbyss x' o heap) e' set

    c@(EPop p as) | all (isKnown heap) as -> returnJust 
        ( ROpt ORPOp
        , StgState
            { code     = c
            , cstack   = stack
            , astack   = duplicateFrame astack
            , heap     = heap
            , settings = set
            }
        )
                  -- Only exists to give better name
                  | otherwise -> irr (ROmega "irreducible EPop") stack astack heap (EPop p as) set
    _ -> irreducible

  where
    irreducible = irr (ROmega "irreducible from omega") stack astack heap code set
    lookupAtom = lookupAtomStack astack
    lookupAtom' a = case lookupAtom a of
        AUnknown _ _ -> a
        x          -> x
    isKnown = isKnownStack astack

beta :: Variable t => ContStack t -> ArgStack t -> Heap t -> 
                           [StgSettings t] -> StgM t (Maybe (Rule, StgState t))
beta cstack@(CtOBranch e brdone brleft:ss) astack h set = case brleft of
    BDef x e   :_ -> omega (ROmega "from beta bdef") cstack 
                            (pushArgs [AUnknown (getCurrentSP astack) x] 
                                      (duplicateFrame astack)) h e set
    BCon c as e:_ -> omega (ROmega "from beta bcon") cstack 
                            (pushArgs (zipWith AUnknown [getCurrentSP astack..] as) 
                                      (duplicateFrame astack)) h e set
    []            -> case e of
        -- kanske ska kommenteras fram?
        ECase _ _ -> omega (RIrr "case in case (from beta)") ss astack h (ECase e brdone) set
        _         -> irr (RIrr "case expression finished (from beta)") ss astack h (ECase e brdone) set

irr :: Variable t => Rule -> ContStack t -> ArgStack t -> Heap t -> Expr t -> 
                          [StgSettings t] -> StgM t (Maybe (Rule, StgState t))
irr rule st ast h e set = returnJust 
    ( rule
    , IrrState 
        { code     = e
        , cstack   = st
        , astack   = ast
        , heap     = h
        , settings = set 
        }
    )

irr' :: Variable t => ContStack t -> ArgStack t -> Heap t -> Expr t -> 
                          [StgSettings t] -> StgM t (Maybe (Rule, StgState t))
irr' (cont : stack) astack heap expr settings = case cont of
    CtOCase brs -> 
       if caseBranches (head settings) 
                            -- need to pop frame, or the scrutinee will be left
        then beta (CtOBranch expr [] brs : stack) (popFrame astack) heap settings
        else case expr of
        -- requires case in case
            ECase _ ibrs | length ibrs == 1 -> -- || explosiveCases settings ->
                         omega (RIrr "cases in case") stack (popFrame astack) heap 
                               (ECase expr brs) settings
            expr' -> case brs of
                [_] -> beta (CtOBranch expr [] brs : stack) (popFrame astack) heap settings
                _   -> irr  (RIrr "case continuation") stack (popFrame astack) 
                            heap (ECase expr' brs) settings
    CtOLet t -> case H.lookupAnywhere t heap of
        Just o -> case mkExpr astack heap expr [t] of
            (astack', expr') -> irr (RIrr "let continuation")
                                    stack astack' heap expr' settings
        Nothing -> error "irr on CtOlet, variable not in abyss!"
    CtOFun args i a -> case expr of
        EAtom at | AVar (Heap v) <- lookupAtomStack astack at
                 , Just (OPap f as) <- H.lookupAnywhere v heap 
                 , Just (OFun bs _ _) <- H.lookupAnywhere f heap -> do
                    let numNewArgs = length bs - length as
                        newArgs    = drop (length bs - numNewArgs) bs
                        sp         = getCurrentSP astack
                    omega (RIrr "papplify") (CtOFun (args ++ newArgs) i a : stack)
                          (pushArgs (zipWith AUnknown [sp..] newArgs) astack)
                          heap (ECall ((Heap f)) (as ++ (zipWith AUnknown [sp..] newArgs))) settings
        _ -> do expr' <- afterburner (getCurrentSP astack) expr
                returnJust (ROpt ORDone
                           , StgState { code     = EAtom (AVar (Heap a))
                                      , heap     = H.insert a (OFun args i expr') heap
                                      , cstack   = stack
                                      , astack   = popFrame astack
                                      , settings = settings
                                      }
                           )
    CtOBranch e brdone (BDef x _ : brleft) ->
        beta (CtOBranch e (brdone ++ [BDef x expr]) brleft : stack)
             (popFrame astack) heap settings
    CtOBranch e brdone (BCon c as _ : brleft) ->
        beta (CtOBranch e (brdone ++ [BCon c as expr]) brleft : stack)
             (popFrame astack) heap settings
    CtOApp f atoms -> do
        v <- newVar
        -- Nånting är ruttet här
        irr (RIrr "app continuation") stack (popFrame astack) heap
            (ECall f atoms) settings
            {-(ELet (NonRec v (OThunk [] {- FEL -} 0 expr))
                  (ECall (Local (getCurrentSP astack) v) atoms)) settings
            -}
    CtOUpd v -> do
        let heap' = H.insertAbyss v (OThunk [] 0 expr ) heap
        irr (RIrr "update (abyss) thunk") stack astack heap' expr settings


psi :: Rule -> ContStack t -> ArgStack t -> Heap t -> Atom t -> [t] 
    -> [StgSettings t] -> StgM t (Maybe (Rule, StgState t))
psi rule st ast h v lbs set = returnJust 
    ( rule
    , PsiState 
        { code     = EAtom v
        , cstack   = st
        , astack   = ast
        , heap     = h
        , letBinds = lbs
        , settings = set 
        }
    )

psi' :: Variable t  => ContStack t -> ArgStack t -> Heap t -> Atom t -> [t] ->
                          [StgSettings t] -> StgM t (Maybe (Rule, StgState t)) 
psi' (cont : stack) astack heap atom lbs settings = case cont of
    CtOLet _ | isKnownStack astack heap atom
        -> psi (RPsi "remove letbinding") stack astack heap atom lbs settings
    CtOLet t -> psi (RPsi "add letbinding") 
                     stack astack heap atom (t:lbs) settings
    CtOCase branch -> case lookupAtomStack astack atom of
        AVar (Heap v) -> case H.lookupAnywhere v heap of
            Just (OCon c atoms) -> case instantiateBranch c atoms branch of
                Nothing -> def
                Just expr -> omg expr atoms
            Just o  -> def
            Nothing -> err
        _ -> def
      where
        omg e as = do
            let astack' = popFrame astack
            omega (RPsi "KnownCase") 
                  (map CtOLet (reverse lbs) ++ stack) 
                  (pushArgs (map (AVar . Heap) lbs) astack') heap
                  (inlt (>= (getCurrentSP astack')) (length lbs)
                        (getCurrentSP astack') as e)
                  settings
        def = case findDefaultBranch atom branch of
            Nothing -> case mkExpr astack heap (EAtom atom) lbs of 
                (astack',e) -> irr (RPsi "psi couldn't inst branch") 
                                   (cont : stack) astack' heap e settings
            Just expr -> omg expr [atom]
        err = error "psi couldn't instantiate that :'/"
    CtUpd t -> case lookupAtomStack astack atom of
        AVar (Heap v) -> case H.lookupHeap v heap of
            Just obj -> psi (RPsi "Can update heap, so do") stack astack
                             (H.insertAbyss t obj heap) atom lbs settings
            _ -> psi (RPsi "we should update, but we can't so we ignore sharing")
                      stack astack heap atom lbs settings
    CtOApp _ as -> case atom of
        AVar v -> do
            let (astack', expr) = mkExpr (popFrame astack) heap (ECall v as) lbs
            omega (RPsi "App continuation") stack astack' heap expr settings
    CtOFun _ _ _ -> do
        let (astack', expr) = mkExpr astack heap (EAtom atom) lbs
        irr (RPsi "fun continuation") (cont : stack) astack' heap expr settings
    CtOBranch _ _ _ -> do
        let (astack', e) = mkExpr astack heap (EAtom atom) lbs
        irr (RPsi "branch continuation") (cont : stack) astack' heap e settings

    _ -> error $ "Psi: I don't know what to do with this stack: " 
     ++ show  (cont:stack)

afterburner :: Variable t => Int -> Expr t -> StgM t (Expr t)
afterburner = (return .) . flip mergeCases []
  where -- k :-> v = Map k v 
    mergeCases :: Variable t => Int -> [(Expr t, (Maybe t, [Atom t]))] -> Expr t -> Expr t
    mergeCases sp substs e = case e of
        ECase exp brs -> case lookup exp substs of
            Nothing -> fun exp brs
            Just (Nothing, vs) -> case brs of
                BDef t' e' : _ -> mergeCases sp substs $ inlt (>= sp) 0 sp vs e' -- byt ut t' mot v i e, ingen ecase langre, shifta
                rest          -> fun exp rest
            Just (Just c,  vs) -> 
                let f brs' = case brs' of
                        BCon c' vs' e' : rest | c == c'  
                             -> mergeCases sp substs $ inlt (>= sp) 0 sp vs e' -- byt ut vs' mot vs
                                              | otherwise -> f rest
                        _ -> fun exp brs
                in f brs
        ELet b@(NonRec _ _) e -> ELet b $ mergeCases (sp + 1) substs e
        ELet b@(Rec bs) e     -> ELet b $ mergeCases (sp + length bs) substs e
        e -> e
      where
        fun exp brs = ECase (mergeCases sp substs exp) 
                [ case br of
                     BCon c vs e -> BCon c vs $ mergeCases (sp+length vs) 
                               ((exp, (Just c, zipWith ((AVar .) . Local) [sp..] vs)) : substs) e
                     BDef t e -> BDef t $ mergeCases (sp + 1) 
                                            -- AVar (Local sp t)
                               ((exp,(Nothing, [AVar (Local sp t)])) : substs) e
                | br <- brs]

{-
    case x of
        I# y -> case x of
            I# z -> e
    <=>
    case x of
        I# y -> shift -1 (>loc(y)) e[y/z]
-}
 

mkExprVar :: Variable t => ArgStack t -> Heap t -> Var t -> [t] -> (ArgStack t, Expr t)
mkExprVar astack h v = mkExpr astack h (EAtom (AVar v)) 

mkExpr :: Variable t => ArgStack t -> Heap t -> Expr t -> [t] -> (ArgStack t, Expr t)
mkExpr astack heap exp = foldr addBindings (astack, exp)
  where
    addBindings var (astack,exp) = (,) (popArg astack) $ case H.lookupAnywhere var heap of
        Just obj -> case getCurrentSP astack - 1 `elem` map rm (localsE exp) of
            True  -> ELet (NonRec var (trUnknown obj)) exp
            False -> inltM [error "mkExpr did something wrong :("] 
                           (getCurrentSP astack - 1) exp
        Nothing  -> case getCurrentSP astack - 1 `elem` map rm (localsE exp) of
            True  -> ELet (NonRec var OBlackhole) exp
            False -> inltM [error "mkExpr did something wrong :("] 
                           (getCurrentSP astack - 1) exp
        
        
        --ELet (NonRec var OBlackhole) exp --exp
    rm (Local i _) = i
    rm _           = error "mkExpr: localsE returned a non-local!"
    
