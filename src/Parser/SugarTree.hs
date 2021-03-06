module Parser.SugarTree where

-- The data type of the language, and auxilliary functions.

data Function t = Function t [t] (Expr t)
  deriving Show

data Expr t    = EAtom (Atom t)
               | ECall t [Expr t]
               | EOpt (Expr t) [Setting t]
               | ECon t [Expr t]
               | ELet Bool [(t,[t],Expr t)] (Expr t)  -- True if recursive
               | ELam [t] (Expr t)
               | ECase (Expr t) [Branch t]
  deriving Show

isAtom (EAtom _) = True
isAtom _         = False

data Setting t = Inlinings (Expr t)
               | Inline t (Expr t)
               | CaseBranches
  deriving Show

data Atom t    = AVar t
               | ANum Integer
               | ADec Double
               | AChr Char
               | AStr String
  deriving Show

data Branch t  = BCon t [t] (Expr t)
               | BDef t (Expr t)
  deriving Show

