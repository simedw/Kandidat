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
  | CtPrint
  | CtPrintCon t [SValue t] [Atom t]
  | CtOpt t
  | CtOFun  [t] t
  | CtOCase [Branch t]
  | CtOLetObj t (Obj t)
  | CtOLetThunk t (Expr t)
  | CtOBranch (Expr t) [Branch t] [Branch t]  
  | CtOInstant Int
 deriving Show

type Stack t = [Cont t]


data StgState t = StgState
  { code  :: Expr  t
  , stack :: Stack t
  , heap  :: Map   t (Obj t)
  , settings :: StgSettings
  }

data StgSettings = StgSettings

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
