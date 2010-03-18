{-# LANGUAGE DeriveDataTypeable #-}
module Stg.AST where 
-- The data type of the language, and auxilliary functions.
--

import Data.Generics
import Data.Generics.PlateData

data Function t = Function t (Obj t)
  deriving (Data, Eq, Show, Typeable)

data Expr t   = EAtom (Atom t)
              | ECall t [Atom t]
              | EPop (Pop t) [Atom t]
              | ELet (Bind t) (Expr t)
              | ECase (Expr t) [Branch t]
              | ESVal (SValue t)
  deriving (Data, Eq, Show, Typeable)

data Bind t
    = NonRec t (Obj t)
    | Rec [(t, Obj t)]
    deriving (Data, Eq, Show, Typeable)

isRecursive :: Bind t -> Bool
isRecursive (Rec _) = True
isRecursive _       = False

getBinds :: Bind t -> [(t, Obj t)]
getBinds (NonRec t obj) = [(t, obj)]
getBinds (Rec binds)    = binds

mkELet :: Bool -> [(t, Obj t)] -> Expr t -> Expr t
mkELet True  = ELet . Rec
mkELet False = flip $ foldr (\(t, obj) e' -> ELet (NonRec t obj) e')

data Pop t    = PBinOp t
                  (Integer -> Integer -> Integer)
                  (Double  -> Double  -> Double)
              | PUnOp t
                  (Integer -> Integer)
                  (Double  -> Double)
              | PBinBool t
                  (Integer -> Integer -> Bool)
                  (Double  -> Double  -> Bool)
  deriving (Data, Typeable)

instance Show t => Show (Pop t) where
  show (PBinOp   op _ _) = show op ++ "#"
  show (PUnOp    op _ _) = show op ++ "#"
  show (PBinBool op _ _) = show op ++ "#"

instance Eq t => Eq (Pop t) where
  PBinOp   op1 _ _ == PBinOp   op2 _ _ = op1 == op2
  PUnOp    op1 _ _ == PUnOp    op2 _ _ = op1 == op2
  PBinBool op1 _ _ == PBinBool op2 _ _ = op1 == op2
  _ == _ = False

isAtom (EAtom _) = True
isAtom _         = False

data Atom t   = AVar t
              | ANum Integer
              | ADec Double
              | AChr Char
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
              | OOpt (Atom t) [Setting t]
  deriving (Data, Eq, Show, Typeable)

data Setting t = Inlinings (Atom t)
               | Inline t (Atom t)
               | CaseBranches
  deriving (Data, Eq, Show, Typeable)

data SValue t 
  = SAtom (Atom t) -- invariant, this is not an variable
  | SCon t [SValue t]
  | SFun
  deriving (Data, Eq, Show, Typeable)
