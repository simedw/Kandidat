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


isKnown :: Ord t => Heap t -> Atom t -> Bool
isKnown h (AVar t) = maybe False (const True) (M.lookup t h)
isKnown _ _        = True

--isBlue :: Ord t => Heap t -> Expr t -> Bool
--isBlue h ex = case ex of
    

omega :: (Ord t, Data t) => t -> Stack t -> Heap t -> Obj t-> StgM t (Maybe (Rule, StgState t))
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
