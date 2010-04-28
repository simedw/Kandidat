{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}
module Stg.Optimise where

import Control.Monad
import Control.Applicative

import Unsafe.Coerce

import "syb" Data.Generics
import Data.Generics.PlateData

{- ta bort -}
import Data.Generics.Biplate
import Data.Generics.Uniplate
{- det har -}
import qualified Data.Set as S

import Data.Map(Map)
import qualified Data.Map as M

import Data.List


import Stg.AST
import Stg.GC
import Stg.Types
import Stg.Rules
import Stg.Branch
import Stg.Substitution
import Stg.Stack
import Stg.Variable

import Stg.Heap (Heap,Location(..))
import qualified Stg.Heap as H

isKnownStack :: Ord t => ArgStack t -> Heap t -> Atom t -> Bool
isKnownStack s h a = case lookupAtomStack s a of
    AVar (Heap t) -> maybe False (const True) (H.lookup t h)
    AUnknown _ _  -> False
    _             -> True

-- OMG it is copy paste from ze interpreter
lookupAtomStack astack (AVar v) = case v of
    Heap x    -> AVar v
    Local x _ -> lookupStackFrame x astack
lookupAtomStack astack (AUnknown i t) = lookupStackFrame i astack
lookupAtomStack astack    a     = a



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

allocObjStack astack o = case o of
    OCon c atoms    -> OCon c (map lookupAtom atoms)
    OThunk fv s exp -> OThunk (map lookupAtom fv) s exp
    OPap   t atoms  -> OPap t (map lookupAtom atoms)
    OOpt alpha set  -> OOpt (lookupAtom alpha) set
    _               -> o
  where
    lookupAtom = lookupAtomStack astack


omega' rule cst ast h e set = returnJust 
    ( rule
    , OmegaState 
        { code     = e
        , cstack   = cst
        , astack   = ast
        , heap     = h
        , settings = set 
        }
    )
{-
reshuffle :: (Data t, Ord t) => Expr t -> StgM t (Expr t)
reshuffle (ECase (ECase e brs1) brs2) = 
        reshuffle =<< (ECase e <$> (mapM
            (\x -> case x of 
                BCon c ts exp -> do 
                    vars <- replicateM (length ts) newVar
                    BCon c vars <$> (reshuffle (ECase (substList ts (map AVar vars) exp) brs2))
                BDef t exp    -> do
                    t' <- newVar
                    BDef t'  <$> (reshuffle (ECase (subst t (AVar t') exp) brs2))
            ) brs1)) 
reshuffle e = return e
-}
{-

    case (case x of
            A a -> x'
            B b -> y'
         )
      X -> r
      Y -> y


    <=>

    case x of
        A a -> case x' of
                 X -> r
                 Y -> y
        B b -> case y' of
                 X -> r
                 Y -> y


-}

omega :: Variable t => ContStack t -> ArgStack t -> Heap t -> Expr t -> 
                            [StgSettings t] -> StgM t (Maybe (Rule, StgState t))
omega stack astack heap code set = case code of
    EAtom a -> case lookupAtom a of
        (AVar (Heap t)) -> case H.locatedLookup t heap of
            Just (OThunk fv _size e, OnHeap) -> do
                returnJust
                    ( ROpt ORKnownAtom
                    , StgState
                        { code     = e
                        , cstack   = CtUpd t : stack
                        , astack   = pushArgs fv $ callFrame astack
                        , heap     = heap
                        , settings = set
                        }
                    )
            Just (OThunk fv _size e, OnAbyss) -> omega' 
                (ROmega "Thunk on abyss") (CtOUpd t : stack) (pushArgs fv $ callFrame astack) heap e set        
            Just (OCon _ _, _) -> case a of
                AVar x -> do 
                    psi' (ROmega "Atom point at Con") stack astack heap x [] set
                _ -> error "impossible, tell me how you did that"
            _ -> irreducible
        AUnknown _ _ -> irreducible
        _ -> case stack of
            CtOCase brs : ss -> case findDefaultBranch a brs of
                Nothing -> error "omega, found atom that has no def branch"
                Just expr -> omega' (ROmega "atom default branch") ss (popFrame astack) heap 
                                    (inltM [a] (getCurrentSP (popFrame astack)) expr) set
        --    om e as = omega' (RPsi "KnownCase") ss (pushArgs as (popFrame ast)) h (inltM as (getCurrentSP (popFrame ast)) e) set
            _ -> error "omega, EATom found that isn't var, and have no case"
    {-ECase (ECase e brs1) brs2 -> do
        code' <- reshuffle code
        omega' (ROmega "case reshuffling") stack heap code' set
    -}
    ECase expr brs -> omega' (ROmega "case") (CtOCase brs : stack) (duplicateFrame astack) heap expr set

    -- Function application with all known arguments,
    -- and with known function.
    ECall f args | isKnown heap (AVar f) && all (isKnown heap) args -> returnJust
        ( ROpt ORKnownCall
        , StgState
            { code  = code
            , cstack = stack
            , astack = astack
            , heap  = heap
            , settings = set
            }
        )

    -- Function application
    ECall (lookupAtom . AVar -> AVar (Heap f')) args | canInline f' set -> let f = Heap f' in
        case H.locatedLookup f' heap of
            -- Known function, inline it!
            Just (OFun as _i e,OnHeap) | length as == length args ->
                  omega' (ROpt ORInline) stack astack heap
                         (inltP (map lookupAtom args) (getCurrentSP astack) e)
                         (inline f' set)
            Just (OPap f as, OnHeap) ->
                omega' (ROmega "apply a pap") stack astack heap (ECall (Heap f) (as ++ args)) set
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
                        { code     = e
                        , cstack   = CtUpd f' : CtOApp args : stack
                        , astack   = astack
                        , heap     = heap
                        , settings = set
                        }
                    )
            -- Unevaluated, abyssimal function, omega the thunk!
            Just (OThunk _as _i e, OnAbyss) -> 
                omega' (ROmega "function abyss thunk") (CtOApp args : stack) astack heap (EAtom $ AVar f) set
            
            -- PAP cases??
            _ -> irreducible
    ELet (NonRec x o@(OCon _ as)) e' | all (isKnown heap) as -> do
        x' <- newVar
        omega' (ROmega "let known con") (CtOLet x' : stack) (pushArgs [AVar $ Heap x'] astack) (H.insert x' o heap) e' set
    ELet (NonRec x o) e' -> do
        x' <- newVar
        omega' (ROmega "let, allocate on abyss") (CtOLet x' : stack) (pushArgs [AVar $ Heap x'] astack) (H.insertAbyss x' (allocObjStack astack o) heap) e' set
    
    c@(EPop p as) | all (isKnown heap) as -> returnJust 
        ( ROpt ORPOp
        , StgState
            { code     = c
            , cstack   = CtOInstant 2 : stack -- 1?
            , astack   = astack
            , heap     = heap
            , settings = set
            }
        )
                  -- WRONG!! If as contains locals that point to unknown theyshould still be locals
                  | otherwise -> irr' (ROmega "irrducieble EPop") stack astack heap (EPop p as) set
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
    irreducible = irr' (ROmega "irreducible from omega") stack astack heap code set
    lookupAtom = lookupAtomStack astack
    lookupAtom' a = case lookupAtom a of
        AUnknown _ _ -> a
        x          -> x
    isKnown = isKnownStack astack

beta :: Variable t => ContStack t -> ArgStack t -> Heap t -> 
                           [StgSettings t] -> StgM t (Maybe (Rule, StgState t))
beta cstack@(CtOBranch e brdone brleft:ss) astack h set = case brleft of
    BDef x e   :_ -> omega cstack (pushArgs [AUnknown (getCurrentSP astack) x] astack) h e set
    BCon c as e:_ -> omega cstack (pushArgs (zipWith AUnknown [getCurrentSP astack..] as) astack) h e set
    []            -> case e of
        -- ECase _ _ -> omega' (RIrr "case in case (from beta)") ss astack h (ECase e brdone) set
        _         -> irr' (RIrr "case expression finished (from beta)") ss (popFrame astack) h (ECase e brdone) set

irr' rule st ast h e set = returnJust 
    ( rule
    , IrrState 
        { code     = e
        , cstack   = st
        , astack   = ast
        , heap     = h
        , settings = set 
        }
    )

findCase :: Variable t => Expr t -> Branch t -> Branch t
findCase e b = case b of 
    BCon c vs expr  -> BCon c vs $ findCaseX b e expr 
    BDef t expr     -> undefined
  where
    findCaseX ::(Ord t, Data t) => Branch t -> Expr t -> Expr t -> Expr t
    findCaseX b@(BCon c vs _) x e'@(ECase e brs) | x == e = case instantiateBranch c (map (AVar . Heap) vs) brs of
        Just x  -> x
        Nothing -> e'
    findCaseX _ _ e = e

irr :: Variable t => ContStack t -> ArgStack t -> Heap t -> Expr t -> 
                          [StgSettings t] -> StgM t (Maybe (Rule, StgState t))
irr (CtOCase brs     : ss) ast h e  set = 
    --irr' (RIrr "magic") ss h (ECase e (map (findCase e) brs)) set
    if caseBranches (head set) 
       then beta (CtOBranch e [] brs:ss) ast h set
       else case e of
        ECase _ _ -> omega' (RIrr "cases in case") ss ast h (ECase e brs) set
        e2 -> case brs of
            [_] -> beta (CtOBranch e [] brs : ss) ast h set
            _   -> irr' (RIrr "case continuation") ss ast h (ECase e2 brs) set
    
irr (CtOLet t        : ss) ast h e set = case H.lookupAnywhere t h of
    Just o  -> case mkExpr h e [t] of
     {-   ELet (NonRec v (OThunk e')) (ECase (EAtom (AVar v')) brs) | v == v'
     --       -> omega' (RIrr "?") (CtOLet t : ss) h (ECase e' brs) set 
     -}
        exp -> irr' (RIrr "let continuation") ss (popArg ast) h exp set -- ELet (NonRec t o) e) set
    Nothing -> error "irr on CtOLet, variable not in abyss!"
  

irr (CtOFun xs i a : ss) as h e set = do
    -- st <- afterburner ss h e set
    returnJust (ROpt ORDone
               , StgState { code = EAtom (AVar (Heap a))
                          , heap = H.insert a (OFun xs i e) h
                          , cstack = ss
                          , astack = popFrame as 
                          , settings = set
                          --, heap = H.insert a (OFun xs (code st)) (heap st)
                          }
               )

irr (CtOBranch e brdone (BDef x _   :brleft) : ss) ast h e' set = 
        beta (CtOBranch e (brdone ++ [BDef x    e']) brleft:ss) ast h set
irr (CtOBranch e brdone (BCon c as _:brleft) : ss) ast h e' set =
        beta (CtOBranch e (brdone ++ [BCon c as e']) brleft:ss) ast h set

irr (CtOApp as : ss) ast h e' set = do
    v <- newVar
    irr' (RIrr "app continuation") ss ast h (ELet (NonRec v (OThunk [] 0 {- naive assumptions -} e')) (ECall (Local (getCurrentSP ast) v) as)) set

irr (CtOUpd v : ss) ast h e set = do
    let h' = H.insertAbyss v (OThunk [] 0 {- naive assumptions -} e) h
    irr' (RIrr "update (abyss) thunk") ss ast h' e set


psi' rule st ast h v lbs set = returnJust 
    ( rule
    , PsiState 
        { code     = EAtom (AVar v)
        , cstack   = st
        , astack   = ast
        , heap     = h
        , letBinds = lbs
        , settings = set 
        }
    )

psi :: Variable t  => ContStack t -> ArgStack t -> Heap t -> Var t -> [t] ->
                          [StgSettings t] -> StgM t (Maybe (Rule, StgState t)) 

-- Brave assumption: The let is dead code
--psi (CtOLet t : ss) h v set = psi' (RPsi "Let continuation") ss h v set
psi (CtOLet t : ss) ast h v@(Heap v') lbs set = case H.lookup v' h of
    Just _  -> psi' (RPsi "remove let, on heap") ss ast h v (t:lbs) set
    Nothing -> psi' (RPsi "add let, on abyss")   ss ast h v (t:lbs) set

psi (CtOLet t : ss) ast h v lbs set = psi' (RPsi "add let, on abyss") ss ast h v (t:lbs) set

psi ss'@(CtOCase branch   : ss) ast h v@(Heap v') lbs set = case H.lookupAnywhere v' h of
    Just (OCon c atoms) -> 
        case instantiateBranch c atoms branch of 
            Nothing -> def
            Just expr -> om expr atoms
    Just o -> def
    Nothing -> err
  where
    err = error "psi couldn't instantiate that :'/"
    om e as = omega' (RPsi "KnownCase") ss (popFrame ast) h (inltM as (getCurrentSP (popFrame ast)) e) set
    --om e as = omega' (RPsi "KnownCase") ss (pushArgs as (popFrame ast)) h e set
    def = case findDefaultBranch (AVar v) branch of
        Nothing -> irr' (RPsi "psi couldn't inst branch") ss' ast h (mkExprVar h v lbs) set
        Just expr -> om expr [AVar v]

psi (CtOUpd t   : ss) ast h v@(Heap v') lbs set = case H.lookupAnywhere v' h of
    Just o -> let h' = H.insertAbyss t o h
               in psi' (RPsi "OUpd thunk") ss ast h' v lbs set
    Nothing -> error $ "psi didn't find that OUpd "

psi (CtUpd t   : ss) ast h (Heap v) lbs set = case H.lookupHeap v h of
    Just o -> let h' = H.insertAbyss t o h
               in psi' (RPsi "OUpd thunk") ss ast h' (Heap v) lbs set
    _ -> error $ "psi CtUpd, is it on the abyss? or not at all???? :O"
    
               
psi ss@(CtOBranch e brdone brleft : _) ast h v lbs set = 
   irr' (RPsi "branch continuation") ss ast h (mkExprVar h v lbs) set
psi ss@(CtOFun args i alpha : _) ast h v lbs set =
    irr' (RPsi "fun continuation") ss ast h (mkExprVar h v lbs) set
psi (CtOApp as : ss) ast h v lbs set = omega' (RPsi "App continutation") ss ast h (mkExpr h (ECall v as) lbs) set

psi s ast h v _ _ = error $ "Psi: I don't know what to do with this stack: " 
     ++ show (unsafeCoerce s :: ContStack String)

{-
afterburner :: (Data t, Ord t) => ContStack t -> Heap t -> 
                                  Expr t -> [StgSettings t] -> StgM t (StgState t)
afterburner stack heap expr set = do
--    let exp' = mergeCases heap expr
--    error $ show (unsafeCoerce expr :: Expr String)
--    error $ show $ sort [ (unsafeCoerce x :: Expr String) | ECase x brs  <- universe expr] 
    return
       (  StgState { cstack   = cstack
                   , heap     = heap
                   , code     = mergeCases expr
                   , settings = set
                   }
       )

  where
    mergeCases :: (Data t, Ord t) => Expr t -> Expr t
    mergeCases (ECase exp brs) = ECase (mergeCases exp) $ map (\x -> case x of
                                            a@(BCon c vs e) -> BCon c vs (substing exp a e)
                                            a@(BDef t e) -> BDef t (substing exp a e)
                                          ) brs
    mergeCases (ELet (NonRec t (OThunk b)) x)      = ELet (NonRec t (OThunk $ mergeCases b)) $ mergeCases  x
    mergeCases (ELet b x)      = ELet b $ mergeCases  x
    mergeCases  x = x


substing :: (Data t, Eq t) => Expr t -> Branch t -> Expr t -> Expr t
substing xr brs = case brs of
    BCon c vs expr -> transformBi (f c vs)
    BDef t expr    -> transformBi (f t [])  
  where
    f c vs e'@(ECase x brs) | x ==  xr = case instantiateBranch c (map AVar vs) brs of
        Just x  -> x
        Nothing -> e'
    f c vs e = e 
-} 

mkExprVar :: Ord t => Heap t -> Var t -> [t] -> Expr t
mkExprVar h v = mkExpr h (EAtom (AVar v)) 

mkExpr :: Ord t => Heap t -> Expr t -> [t] -> Expr t
mkExpr heap = foldr addBindings
  where
    addBindings var exp = case H.lookupAbyss var heap of
        Just obj -> case True of -- var `S.member` freeVars exp of
            True -> ELet (NonRec var (trUnknown obj)) exp
            False -> exp
        Nothing  -> ELet (NonRec var OBlackhole) exp --exp
