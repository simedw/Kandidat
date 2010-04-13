{-# LANGUAGE PackageImports #-}
module Stg.Types where

import "mtl" Control.Monad.State
import "mtl" Control.Monad.Writer


import Data.Map(Map)
import qualified Data.Map as M


import Stg.AST
import Stg.Rules

import Stg.Stack

import Stg.Heap
import qualified Stg.Heap as H

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
  | CtOLet t
  | CtOBranch (Expr t) [Branch t] [Branch t]  
  | CtOUpd t
  | CtOInstant Int
 deriving Show

type ContStack t = [Cont t]

data StgState t 
  = StgState
      { code     :: Expr  t
      , cstack   :: ContStack t
      , astack   :: ArgStack t 
      , heap     :: Heap  t
      , settings :: [StgSettings t]
      }
  | OmegaState
      { code     :: Expr  t
      , cstack   :: ContStack t
      , astack   :: ArgStack t
      , heap     :: Heap  t
      , settings :: [StgSettings t]
      }
  | PsiState
      { code     :: Expr  t
      , cstack   :: ContStack t
      , astack   :: ArgStack t
      , heap     :: Heap  t
      , letBinds :: [t]
      , settings :: [StgSettings t]
      }
  | IrrState
      { code     :: Expr  t
      , cstack   :: ContStack t
      , astack   :: ArgStack t
      , heap     :: Heap  t
      , settings :: [StgSettings t]
      }
  

data StgSettings t = StgSettings
  { globalInline :: Integer
  , inlines      :: Map t Integer
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
    lookupInteger (AVar (Heap v)) = case H.lookup v h of
        Just (OCon _ [ANum x]) -> x
        Nothing -> error "makeSettings, invalid arguments to optimise with"
    lookupInteger (AVar (Local i v)) = error "måste ta in callstacken också, TODO :)"

data StgMState t = StgMState
    { nameSupply :: [t]
    , mkCons     :: t -> t
    }

type StgM t a = State (StgMState t) a


evalStgM :: StgM t a -> StgMState t -> a
evalStgM = evalState

runStgM :: StgM t a -> StgMState t -> (a, StgMState t)
runStgM m s = runState m s
            
-- | Create a fresh unbound variable
newVar :: StgM t t
newVar = do
    st@(StgMState { nameSupply = (n:ns) }) <- get
    put $ st { nameSupply = ns }
    return n

returnJust x = return (Just x)
