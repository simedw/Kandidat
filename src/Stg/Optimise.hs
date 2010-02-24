module Stg.Optimise where

import Control.Monad

import Data.Map(Map)
import qualified Data.Map as M

import Stg.AST
import Stg.Types
import Stg.Rules


isKnown :: Ord t => Heap t -> Atom t -> Bool
isKnown h (AVar t) = maybe False (const True) (M.lookup t h)
isKnown _ _        = True


omega :: Ord t => t -> Stack t -> Heap t -> Obj t-> StgM t (Maybe (Rule, StgState t))
omega alpha stack heap obj@(OFun args code) = case code of
    ECase e brs -> case e of
        ECall f as | all (isKnown heap) as -> do
            t <- newVar
            let heap'  = M.insert t (OThunk e) heap
                heap'' = M.insert alpha (OFun args (ECase (EAtom (AVar t)) brs)) heap'
            returnJust
                ( ROpt ORKnownCall
                , StgState 
                    { code  = EAtom (AVar t)
                    , stack = CtContOpt f : stack
                    , heap  = heap'' }
                )
    _ -> returnJust
        ( ROpt ORDone
        , StgState
            { code  = EAtom (AVar alpha)
            , stack = stack
            , heap  = M.insert alpha obj heap
            }
        )