{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PackageImports #-}
module Stg.AST where 
-- The data type of the language, and auxilliary functions.

import "syb" Data.Generics
import Data.Generics.PlateData
import Data.Function
import Shared.Primitives

data Function t = Function t (Obj t)
  deriving (Data, Eq, Show, Typeable)

data Expr t   = EAtom (Atom t)
              | ECall (Var t) [Atom t]
              | EPop (Primitive t) [Atom t]
              | ELet (Bind t) (Expr t)
              | ECase (Expr t) [Branch t]
              | ESVal (SValue t)
  deriving (Data, Eq, Ord, Show, Typeable)

data Bind t
    = NonRec t (Obj t)
    | Rec [(t, Obj t)]
    deriving (Data, Eq, Ord, Show, Typeable)

isRecursive :: Bind t -> Bool
isRecursive (Rec _) = True
isRecursive _       = False

getBinds :: Bind t -> [(t, Obj t)]
getBinds (NonRec t obj) = [(t, obj)]
getBinds (Rec binds)    = binds

mkELet :: Bool -> [(t, Obj t)] -> Expr t -> Expr t
mkELet True  = ELet . Rec
mkELet False = flip $ foldr (ELet . uncurry NonRec)
 

data Var t = Heap t
           | Local Int t
  deriving (Show, Eq, Ord, Data, Typeable)

isLocal (Local _ _) = True
isLocal _           = False

isVar (AVar _) = True
isVar _        = False


isAtom (EAtom _) = True
isAtom _         = False

data Atom t   = AVar (Var t)
              | ANum Integer
              | ADec Double
              | AChr Char
              | AUnknown Int t
  deriving (Data, Eq, Ord, Show, Typeable)


data Branch t = BCon t [t] (Expr t)
              | BDef t (Expr t)
  deriving (Data, Eq, Ord, Show, Typeable)

data Obj t    =   -- args, size, body
                OFun [t]   Int   (Expr t)
              | OPap t [Atom t]
              | OCon t [Atom t]
                    -- free vars, size, body
              | OThunk [Atom t]   Int   (Expr t)
              | OBlackhole
              | OOpt (Atom t) [Setting t]
  deriving (Data, Eq, Ord, Show, Typeable)

data Setting t = Inlinings (Atom t)
               | Inline t (Atom t)
               | CaseBranches
  deriving (Data, Eq, Ord, Show, Typeable)

data SValue t 
  = SAtom (Atom t) -- invariant, this is not an variable
  | SCon t [SValue t]
  | SFun
  deriving (Data, Eq, Ord, Show, Typeable)
