{-# LANGUAGE DeriveDataTypeable #-}
module Stg.AST where 
-- The data type of the language, and auxilliary functions.

import Data.Generics
import Data.Generics.PlateData


data Function t = Function t (Obj t)
  deriving (Data, Eq, Show, Typeable)

data Expr t   = EAtom (Atom t)
              | ECall t [Atom t]
              | ELet Bool [(t,Obj t)] (Expr t)  -- True if recursive
              | ECase (Expr t) [Branch t]
  deriving (Data, Eq, Show, Typeable)

isAtom (EAtom _) = True
isAtom _         = False

data Atom t   = AVar t
              | ANum Integer
  deriving (Data, Eq, Show, Typeable)

data Branch t = BCon t [t] (Expr t)
              | BDef t (Expr t)
  deriving (Data, Eq, Show, Typeable)

data Obj t    = OFun [t] (Expr t)
              | OPap (Obj t) [Atom t]   
              | OCon t [Atom t]        
              | OThunk (Expr t)
              | OBlackhole
  deriving (Data, Eq, Show, Typeable)

