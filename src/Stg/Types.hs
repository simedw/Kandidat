{-# LANGUAGE PackageImports #-}
module Stg.Types where

import "mtl" Control.Monad.State
import "mtl" Control.Monad.Writer

import Data.Map(Map)
import qualified Data.Map as M

import Stg.AST
import Stg.Rules

type Heap  t = Map t (Obj t)

data Cont t 
  = CtCase [Branch t]
  | CtUpd t
  | CtArg (Atom t)
  | CtPrint
  | CtPrintCon t [SValue t] [Atom t]
  | CtOpt t
  | CtOFun  [t] t
  | CtOApp [Atom t]
  | CtOCase [Branch t]
  | CtOLetObj t (Obj t)
  | CtOLetThunk t (Expr t)
  | CtOBranch (Expr t) [Branch t] [Branch t]  
  | CtOInstant Int
 deriving Show

type Stack t = [Cont t]


data StgState t 
  = StgState
      { code  :: Expr  t
      , stack :: Stack t
      , heap  :: Map   t (Obj t)
      , settings :: [StgSettings t]
      }
  | OmegaState
      { code  :: Expr  t
      , stack :: Stack t
      , heap  :: Map   t (Obj t)
      , settings :: [StgSettings t]
      }
  | PsiState
      { code  :: Expr t
      , stack :: Stack t
      , heap  :: Map   t (Obj t)
      , settings :: [StgSettings t]
      }
  | IrrState
      { code  :: Expr  t
      , stack :: Stack t
      , heap  :: Map   t (Obj t)
      , settings :: [StgSettings t]
      }
  

data StgSettings t = StgSettings
  { globalInline :: Integer
  , inlines  :: Map t Integer
  , caseBranches :: Bool
  }

canInline :: Ord t => t -> [StgSettings t] -> Bool
canInline v (set@StgSettings { inlines = inlines, globalInline = glob } : sets) 
    = case M.lookup v inlines of
        Just   0 -> False
        Just   n -> True
        Nothing -> glob /= 0
        
inline :: Ord t => t -> [StgSettings t] -> [StgSettings t]
inline v (set@StgSettings { inlines = inlines, globalInline = glob } : sets)
    = case M.member v inlines of
        True  -> decrementInline v set : sets
        False -> decrementGlobal set : sets
        
defaultOptSettings :: StgSettings t
defaultOptSettings = StgSettings (-1) M.empty False

decrementGlobal :: StgSettings t -> StgSettings t
decrementGlobal s = s { globalInline = globalInline s - 1 }

decrementInline :: Ord t => t -> StgSettings t -> StgSettings t
decrementInline f s = s 
    { inlines = M.update (Just . (subtract 1)) f (inlines s)  }

makeSettings :: Ord t => Heap t -> [Setting t] -> StgSettings t
makeSettings h = foldr
    (\setting s -> case setting of
          Inlinings a  -> s { globalInline = lookupInteger a }
          Inline f a   -> s { inlines = M.insert f (lookupInteger a) (inlines s) }
          CaseBranches -> s { caseBranches = True } )
    defaultOptSettings
  where
    lookupInteger (ANum x) = x
    lookupInteger (AVar v) = case M.lookup v h of
        Just (OCon _ [ANum x]) -> x
        Nothing -> error "makeSettings, invalid arguments to optimise with"

data StgMState t = StgMState
    { nameSupply :: [t]
    , mkCons     :: String -> t
    }

type StgM t a = StateT (StgMState t) (Writer [String]) a

unnest :: ((a, b), c) -> (a, b, c)
unnest    ((a, b), c) =  (a, b, c)

runStgM :: StgM t a -> StgMState t -> a
runStgM m s= fst . runWriter $ evalStateT m s

ruleStgM :: StgM t a -> StgMState t -> (a, StgMState t, [String])
ruleStgM m s = unnest . runWriter $ runStateT m s
            
traceStgM :: StgM t a -> StgMState t -> [String]
traceStgM m s = snd . runWriter $ runStateT m s

output :: String -> StgM t ()
output s = tell [s]

-- | Create a fresh unbound variable
newVar :: StgM t t
newVar = do
    st@(StgMState { nameSupply = (n:ns) }) <- get
    put $ st { nameSupply = ns }
    return n

returnJust x = return (Just x)
