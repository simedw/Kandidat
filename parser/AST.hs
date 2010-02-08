module AST where

-- The data type of the language, and auxilliary functions.

data Function t = Function t (Obj t)
  deriving Show

data Expr t   = EAtom (Atom t)
              | ECall t [Atom t]
              | ELet Bool [(t,Obj t)] (Expr t)  -- True if recursive
              | ECase (Expr t) [Branch t]
  deriving Show

isAtom (EAtom _) = True
isAtom _         = False

data Atom t   = AVar t
              | ANum Integer
  deriving Show

data Branch t = BCon t [t] (Expr t)
              | BDef t (Expr t)
  deriving Show

data Obj t    = OFun [t] (Expr t)
              | OPap (Obj t) [Atom t]   
              | OCon t [Atom t]        
              | OThunk (Expr t)
              | OBlackhole
  deriving Show
