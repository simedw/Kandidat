{-# LANGUAGE RecordPuns #-}
module Stg.Interpreter where

-- Naive Stg interpreter

import Stg.AST
import Stg.Substitution

import Data.Map

data Stack t 
  = StCase [Branch t]
  | StUpd t
  | StArg (Atom t)

newtype StgState t = StgState
  { code  :: Expr  t
  , stack :: Stack t
  , heap  :: Map   t (Obj t)
  , names :: [t]
  }

step :: StgState t -> StgState t
  step st{code, stack, heap, names} = case code of
  ELet  b defs e -> undefined
  ECase e br     -> undefined
  ECall i args   -> undefined
  EAtom at       -> undefined
