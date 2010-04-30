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

-- OMG it is copy paste from ze interpreter :)
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
                        , astack   = astack
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

    ECase expr brs -> omega (ROmega "case") (CtOCase brs : stack) 
                            (duplicateFrame astack) heap expr set

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
    ECall (lookupAtom . AVar &&& id -> (AVar (Heap f'), v)) args | canInline f' set -> let f = Heap f' in
        case H.locatedLookup f' heap of
            -- Known function, inline it!
            Just (OFun as _i e,OnHeap) | length as == length args ->
                  omega (ROpt ORInline) stack astack heap
                        (inltP args (getCurrentSP astack) e)
                        (inline f' set)
            Just (OPap f as, OnHeap) ->
                omega (ROmega "apply a pap") stack astack heap (ECall (Heap f) (as ++ args)) set
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
                        , astack   = duplicateFrame astack -- <-
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
    ELet (NonRec x o@(OCon _ as)) e' | all (isKnown heap) as -> do
        x' <- newVar
        omega (ROmega "let known con") (CtOLet x' : stack) 
              (pushArgs [AVar $ Heap x'] astack) (H.insert x' o heap) e' set
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
            , astack   = astack
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
                            (pushArgs [AUnknown (getCurrentSP astack) x] astack) h e set
    BCon c as e:_ -> omega (ROmega "from beta bcon") cstack 
                            (pushArgs (zipWith AUnknown [getCurrentSP astack..] as) astack) h e set
    []            -> case e of
        -- ECase _ _ -> omega (RIrr "case in case (from beta)") ss astack h (ECase e brdone) set
        _         -> irr (RIrr "case expression finished (from beta)") ss (popFrame astack) h (ECase e brdone) set

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
        then beta (CtOBranch expr [] brs : stack) astack heap settings
        else case expr of
        -- requires case in case
        --ECase _ _ -> omega (RIrr "cases in case") ss ast h (ECase e brs) set
            expr' -> case brs of
                [_] -> beta (CtOBranch expr [] brs : stack) astack heap settings
                _   -> irr  (RIrr "case continuation") stack (popFrame astack) 
                            heap (ECase expr' brs) settings
    CtOLet t -> case H.lookupAnywhere t heap of
        Just o -> case mkExpr astack heap expr [t] of
            (astack', expr') -> irr (RIrr "let continuation")
                                    stack astack' heap expr' settings
        Nothing -> error "irr on CtOlet, variable not in abyss!"
    CtOFun args i a -> do
        returnJust (ROpt ORDone
                   , StgState { code     = EAtom (AVar (Heap a))
                              , heap     = H.insert a (OFun args i expr) heap
                              , cstack   = stack
                              , astack   = popFrame astack
                              , settings = settings
                              }
                   )
    CtOBranch e brdone (BDef x _ : brleft) ->
        beta (CtOBranch e (brdone ++ [BDef x expr]) brleft : stack)
             (popArg astack) heap settings
    CtOBranch e brdone (BCon c as _ : brleft) ->
        beta (CtOBranch e (brdone ++ [BCon c as expr]) brleft : stack)
             (popArgs (length as) astack) heap settings
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
                  (map CtOLet lbs ++ stack) 
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




{-
-- Brave assumption: The let is dead code
--psi (CtOLet t : ss) h v set = psi (RPsi "Let continuation") ss h v set
psi (CtOLet t : ss) ast h v@(Heap v') lbs set = case H.lookup v' h of
    Just _  -> psi (RPsi "remove let, on heap") ss ast h v (t:lbs) set
    Nothing -> psi (RPsi "add let, on abyss")   ss ast h v (t:lbs) set

psi (CtOLet t : ss) ast h v lbs set = psi (RPsi "add let, on abyss") ss ast h v (t:lbs) set
 
psi ss'@(CtOCase branch   : ss) ast h (lookupAtomStack ast . AVar -> AVar (Heap v')) lbs set = 
    case H.lookupAnywhere v' h of
        Just (OCon c atoms) -> 
            case instantiateBranch c atoms branch of 
                Nothing -> def
                Just expr -> om expr atoms
        Just o -> def
        Nothing -> err
  where
    v = Heap v'
    err = error "psi couldn't instantiate that :'/"
                -- daniels ide om att oka shiftningen till let
    om e as = let ast1:ast2:astrest = ast -- head ast : drop 2 ast
                  ast' = ast2:astrest
              in  omega (RPsi "KnownCase") 
                         (map CtOLet lbs ++ ss) 
                         (pushArgs (map (AVar . Heap) lbs) ast') h 
                         (inlt (>= (getCurrentSP [ast2])) (length lbs) 
                               (getCurrentSP [ast2]) as e)
                         set
                      
--                     inlt (>= (getCurrentSP ast')) (length lbs) 0 []
--                     $ inltM as (getCurrentSP ast') e) set

    -- THIS IS SZISSLYING TIME, shizzle time, shizzle time, now we have
    -- a shizzle time, chisel time, let's do this thing

    --om e as = omega (RPsi "KnownCase") ss (pushArgs as (popFrame ast)) h e set
    def = case findDefaultBranch (AVar v) branch of
        Nothing -> case mkExprVar ast h v lbs of 
            (ast',e) -> irr' (RPsi "psi couldn't inst branch") ss' ast' h e set
        Just expr -> om expr [AVar v]

-- is this obsolete??
psi (CtOUpd t   : ss) ast h (lookupAtomStack ast . AVar -> AVar (Heap v')) lbs set = 
    let v = Heap v' in case H.lookupAnywhere v' h of
        Just o -> let h' = H.insertAbyss t o h
                  in psi (RPsi "OUpd thunk") ss ast h' v lbs set
        Nothing -> error $ "psi didn't find that OUpd "

psi (CtOUpd t   : ss) ast h v@(Local i x) lbs set = error "psi on CtOUpd and Local"

psi (CtUpd t   : ss) ast h (Heap v) lbs set = case H.lookupHeap v h of
    Just o -> let h' = H.insertAbyss t o h
               in psi (RPsi "OUpd thunk") ss ast h' (Heap v) lbs set
    _ -> error $ "psi CtUpd, is it on the abyss? or not at all???? :O"
    
               
psi ss@(CtOBranch e brdone brleft : _) ast h v lbs set = 
    irr' (RPsi "branch continuation") ss ast' h e set
  where (ast', e) = mkExprVar ast h v lbs
psi ss@(CtOFun args i alpha : _) ast h v lbs set =
     irr' (RPsi "fun continuation") ss ast' h e set
  where (ast', e) = mkExprVar ast h v lbs
psi (CtOApp as : ss) ast h v lbs set = omega (RPsi "App continutation") ss ast' h e set
    where (ast', e) = mkExpr (popFrame ast) h (ECall v as) lbs

psi s ast h v _ _ = error $ "Psi: I don't know what to do with this stack: " 
     ++ show s -- (unsafeCoerce s :: ContStack String)

-}

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

mkExprVar :: Variable t => ArgStack t -> Heap t -> Var t -> [t] -> (ArgStack t, Expr t)
mkExprVar astack h v = mkExpr astack h (EAtom (AVar v)) 

mkExpr :: Variable t => ArgStack t -> Heap t -> Expr t -> [t] -> (ArgStack t, Expr t)
mkExpr astack heap exp = foldr addBindings (astack, exp)
  where
    addBindings var (astack,exp) = (,) (popArg astack) $ case H.lookupAbyss var heap of
        Just obj -> case getCurrentSP astack - 1 `elem` map rm (localsE exp) of
            True ->  ELet (NonRec var (trUnknown obj)) exp
            False -> inltM [error "mkExpr did something wrong :("] (getCurrentSP astack - 1) exp
        Nothing  -> case getCurrentSP astack - 1 `elem` map rm (localsE exp) of
            True ->  ELet (NonRec var OBlackhole) exp
            False -> inltM [error "mkExpr did something wrong :("] (getCurrentSP astack - 1) exp
        
        
        --ELet (NonRec var OBlackhole) exp --exp
    rm (Local i _) = i
    rm _           = error "mkExpr: localsE returned a non-local!"
    