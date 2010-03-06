module Stg.Optimise where

import Control.Monad

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
    ELet (NonRec x (OThunk e))  e' -> omega (CtOLetThunk x e' : stack) heap e
    ELet (NonRec x (OCon c as)) e' 
        | all (isKnown heap) as -> do
            x' <- newVar
            omega stack (M.insert x' (OCon c as) heap) (subst x (AVar x') e')
        | otherwise -> omega (CtOLetObj x (OCon c as):stack) heap e'           

    _ -> irreducible
  where
    irreducible = irr stack heap code

irr :: (Ord t, Data t) => Stack t -> Heap t -> Expr t -> StgM t (Maybe (Rule, StgState t))
irr (CtOLetThunk x e : ss) h e' = omega (CtOLetObj x (OThunk e') : ss) h e
irr (CtOCase brs     : ss) h e  = irr ss h (ECase e brs)
irr (CtOLetObj x o   : ss) h e  = irr ss h (ELet (NonRec x o) e)
irr (CtOFun xs a     : ss) h e  = do 
    let h' = M.insert a (OFun xs e) h 
    returnJust
         ( ROpt ORDone
         , StgState
            { code  = EAtom (AVar a) --lpha
            , stack = ss
            , heap  = h'
            }
         )


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

--psi (CtOInstant     : ss) h v = omega ss h e
{-
omega alpha stack heap obj@(OFun args code) = case code of
    ECase e brs -> case e of
        EAtom v@(AVar x) | isKnown heap v -> do
            case M.lookup x heap of
                Just (OCon c as) -> case instantiateBranch c as brs of
                    Nothing -> case findDefaultBranch v brs of
                        Nothing -> error "defect :("
                        Just e' -> rknowncase e'
                    Just e' -> rknowncase e'
                Just (OThunk _) -> do
                    returnJust
                        ( ROpt ORCaseThunk
                        , StgState
                            { code = e
                            , stack = CtContOpt alpha : stack
                            , heap = M.insert alpha obj heap
                            }
                        )
                Just fun -> do
                    case findDefaultBranch v brs of
                        Nothing -> error "omega.ecase.eatom.nocon.nodefaultbranch"
                        Just e' -> rknowncase e'
                _               -> done
        ECall f as | all (isKnown heap) as -> do
            t <- newVar
            let heap'  = M.insert t (OThunk e) heap
                heap'' = M.insert alpha (OFun args (ECase (EAtom (AVar t)) brs)) heap'
            returnJust
                ( ROpt ORKnownCall
                , StgState 
                    { code  = EAtom (AVar t)
                    , stack = CtContOpt alpha : stack
                    , heap  = heap'' }
                )
        ELet b binds expr -> done
        _ -> done
    _ -> done
  where
    rknowncase expr = 
        let fun = OFun args expr
        in returnJust
        ( ROpt ORKnownCase
        , StgState
            { code  = EAtom (AVar alpha)
            , stack = CtContOpt alpha : stack
            , heap  = M.insert alpha fun heap
            }
        )
    done = 
        returnJust
            ( ROpt ORDone
            , StgState
                { code  = EAtom (AVar alpha)
                , stack = stack
                , heap  = M.insert alpha obj heap
                }
            )
-}
