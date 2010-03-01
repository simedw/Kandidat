{-# LANGUAGE PackageImports #-}
module Stg.Types where

import "mtl" Control.Monad.State

import Data.Map(Map)
import qualified Data.Map as M

import Stg.AST

type Heap  t = Map t (Obj t)

data Cont t 
  = CtCase [Branch t]
  | CtUpd t
  | CtArg (Atom t)
  | CtOpt t
  | CtContOpt t
 deriving Show

type Stack t = [Cont t]


data StgState t = StgState
  { code  :: Expr  t
  , stack :: Stack t
  , heap  :: Map   t (Obj t)
  }
 -- deriving Show

data StgMState t = StgMState
    { nameSupply :: [t]
    , mkCons     :: String -> t
    }

type StgM t = State (StgMState t)

-- | Create a fresh unbound variable
newVar :: StgM t t
newVar = do
    st@(StgMState { nameSupply = (n:ns) }) <- get
    put $ st { nameSupply = ns }
    return n

returnJust x = return (Just x)
