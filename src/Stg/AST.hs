{-# LANGUAGE DeriveDataTypeable #-}
module Stg.AST where 
-- The data type of the language, and auxilliary functions.

import Data.Generics
import Data.Generics.PlateData

data Function t = Function t (Obj t)
  deriving (Data, Eq, Show, Typeable)

data Expr t   = EAtom (Atom t)
              | ECall t [Atom t]
              | EPop Pop [Atom t]
              | ELet Bool [(t,Obj t)] (Expr t)  -- True if recursive
              | ECase (Expr t) [Branch t]
  deriving (Data, Eq, Show, Typeable)

data Pop      = PAdd | PSub | PMul | PDiv | PMod
              | PLe  | PLt  | PGe  | PGt  | PEq
         deriving (Data, Typeable, Eq, Show)

isAtom (EAtom _) = True
isAtom _         = False

data Atom t   = AVar t
              | ANum Integer
              | ADec Double
  deriving (Data, Eq, Show, Typeable)

isVar (AVar _) = True
isVar _        = False

data Branch t = BCon t [t] (Expr t)
              | BDef t (Expr t)
  deriving (Data, Eq, Show, Typeable)

data Obj t    = OFun [t] (Expr t)
              | OPap t [Atom t]   
              | OCon t [Atom t]        
              | OThunk (Expr t)
              | OBlackhole
  deriving (Data, Eq, Show, Typeable)

