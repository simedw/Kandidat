module Stg.Optimise where

import Control.Monad
import Control.Applicative

import Unsafe.Coerce

import Data.Generics
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
            psi' (ROmega "Atom point at Con") stack heap t [] set
        _ -> irreducible
    EAtom a -> case stack of
        CtOCase brs : ss -> case findDefaultBranch a brs of
            Nothing -> error "omega, found atom that has no def branch"
            Just expr -> omega' (ROmega "atom default branch") ss heap expr set
        _ -> error "omega, EATom found that isn't var, and have no case"
    ECase (ECase e brs1) brs2 -> do
        code' <- reshuffle code
        omega' (ROmega "case reshuffling") stack heap code' set

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
    ELet (NonRec x o@(OCon _ as)) e' | all (isKnown heap) as -> do
        x' <- newVar
        omega' (ROmega "let known con") (CtOLet x' : stack) (H.insert x' o heap) (subst x (AVar x') e') set
    ELet (NonRec x o) e' -> do
        x' <- newVar
        omega' (ROmega "let, allocate on abyss") (CtOLet x' : stack) (H.insertAbyss x' o heap) (subst x (AVar x') e') set
    
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
    []            -> case e of
        ECase _ _ -> omega' (RIrr "case in case (from beta)") ss h (ECase e brdone) set
        _         -> irr' (RIrr "case expression finished (from beta)") ss h (ECase e brdone) set

irr' rule st h e set = returnJust 
    ( rule
    , IrrState 
        { code     = e
        , stack    = st
        , heap     = h
        , settings = set 
        }
    )

findCase :: (Ord t, Data t) => Expr t -> Branch t -> Branch t
findCase e b = case b of 
    BCon c vs expr  -> BCon c vs $ findCaseX b e expr 
    BDef t expr     -> undefined
  where
    findCaseX ::(Ord t, Data t) => Branch t -> Expr t -> Expr t -> Expr t
    findCaseX b@(BCon c vs _) x e'@(ECase e brs) | x == e = case instantiateBranch c (map AVar vs) brs of
        Just x  -> x
        Nothing -> e'
    findCaseX _ _ e = e

irr :: (Ord t, Data t) => Stack t -> Heap t -> Expr t -> 
                          [StgSettings t] -> StgM t (Maybe (Rule, StgState t))
irr (CtOCase brs     : ss) h e  set = 
    --irr' (RIrr "magic") ss h (ECase e (map (findCase e) brs)) set
    if caseBranches (head set) 
       then beta (CtOBranch e [] brs:ss) h set
       else case e of
        ECase _ _ -> omega' (RIrr "cases in case") ss h (ECase e brs) set
        e2 -> case brs of
            [_] -> beta (CtOBranch e [] brs : ss) h set
            _   -> irr' (RIrr "case continuation") ss h (ECase e2 brs) set
      
irr (CtOLet t        : ss) h e set = case H.lookupAnywhere t h of
    Just o  -> irr' (RIrr "let continuation") ss h (mkExpr h e [t]) set -- ELet (NonRec t o) e) set
    Nothing -> error "irr on CtOLet, variable not in abyss!"
  

irr (CtOFun xs a   : ss) h e set = do
    st <- afterburner ss h e set
    returnJust (ROpt ORDone
               , st { code = EAtom (AVar a)
                    , heap = H.insert a (OFun xs (code st)) (heap st)
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

psi' rule st h v lbs set = returnJust 
    ( rule
    , PsiState 
        { code     = EAtom (AVar v)
        , stack    = st
        , heap     = h
        , letBinds = lbs
        , settings = set 
        }
    )

psi :: (Ord t, Data t) => Stack t -> Heap t -> t -> [t] ->
                          [StgSettings t] -> StgM t (Maybe (Rule, StgState t)) 

-- Brave assumption: The let is dead code
--psi (CtOLet t : ss) h v set = psi' (RPsi "Let continuation") ss h v set
psi (CtOLet t : ss) h v lbs set = case H.lookup v h of
    Just _  -> psi' (RPsi "remove let, on heap") ss h v lbs set
    Nothing -> psi' (RPsi "add let, on abyss")   ss h v (t:lbs) set

psi ss'@(CtOCase branch   : ss) h v lbs set = case H.lookupAnywhere v h of
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
        Nothing -> irr' (RPsi "psi couldn't inst branch") ss' h (mkExprVar h v lbs) set
        Just expr -> om expr
psi (CtOUpd t   : ss) h v lbs set = case H.lookupAnywhere v h of
    Just o -> let h' = H.insertAbyss t o h
               in psi' (RPsi "OUpd thunk") ss h' v lbs set
    Nothing -> error $ "psi didn't find that OUpd "
psi (CtUpd t   : ss) h v lbs set = case H.lookupHeap v h of
    Just o -> let h' = H.insertAbyss t o h
               in psi' (RPsi "OUpd thunk") ss h' v lbs set
    _ -> error $ "psi CtUpd, is it on the abyss? or not at all???? :O"
    
               
psi ss@(CtOBranch e brdone brleft : _) h v lbs set = 
   irr' (RPsi "branch continuation") ss h (mkExprVar h v lbs) set
psi ss@(CtOFun args alpha : _) h v         lbs set =
    irr' (RPsi "fun continuation") ss h (mkExprVar h v lbs) set
psi (CtOApp as : ss) h v lbs set = omega' (RPsi "App continutation") ss h (mkExpr h (ECall v as) lbs) set

psi s h v _ _ = error $ "Psi: I don't know what to do with this stack: " 
     ++ show (unsafeCoerce s :: Stack String)



afterburner :: (Data t, Ord t) => Stack t -> Heap t -> 
                                  Expr t -> [StgSettings t] -> StgM t (StgState t)
afterburner stack heap expr set = do
--    let exp' = mergeCases heap expr
--    error $ show (unsafeCoerce expr :: Expr String)
--    error $ show $ sort [ (unsafeCoerce x :: Expr String) | ECase x brs  <- universe expr] 
    return
       (  StgState { stack    = stack
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
 

mkExprVar :: Ord t => Heap t -> t -> [t] -> Expr t
mkExprVar h v = mkExpr h (EAtom (AVar v)) 

mkExpr :: Ord t => Heap t -> Expr t -> [t] -> Expr t
mkExpr heap = foldr addBindings
  where
    addBindings var exp = case H.lookupAbyss var heap of
        Just obj -> case var `S.member` freeVars exp of
            True -> ELet (NonRec var obj) exp
            False -> exp
        Nothing  -> exp
