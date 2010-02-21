{-# LANGUAGE PackageImports #-}
module Stg.Types where

import "mtl" Control.Monad.State

import Data.Map(Map)
import qualified Data.Map as M

import Text.PrettyPrint


import Parser.Pretty.Pretty
import Stg.AST

type Heap  t = Map t (Obj t)

data Cont t 
  = CtCase [Branch t]
  | CtUpd t
  | CtArg (Atom t)
  | CtOpt t
 deriving Show

type Stack t = [Cont t]


data StgState t = StgState
  { code  :: Expr  t
  , stack :: Stack t
  , heap  :: Map   t (Obj t)
  }
 -- deriving Show

type StgM t = State [t]

-- | Create a fresh unbound variable
newVar :: StgM t t
newVar = do
    v <- get
    put (tail v)
    return (head v)

instance Show t => Show (StgState t) where
  show st@(StgState code stack heap) = 
    "stack: " ++ show stack 
    ++ "\ncode: " ++ show (prExpr (text . show) code)
    ++ "\nheap: " ++ concat [ show (id, prObj (text . show) obj) ++ "\n\t"
                             | (id, obj) <- M.toList heap]
