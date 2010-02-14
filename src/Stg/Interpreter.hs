{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PackageImports #-}
module Stg.Interpreter where

-- Naive Stg interpreter

import Data.Generics

import Data.Map(Map)
import qualified Data.Map as M
import "mtl" Control.Monad.State

import Stg.AST
import Stg.Rules
import Stg.Substitution


data Cont t 
  = CtCase [Branch t]
  | CtUpd t
  | CtArg (Atom t)

type Stack t = [Cont t]
type Heap  t = Map t (Obj t)

type StgM t = State [t]

-- | Create a fresh unbound variable
newVar :: StgM t t
newVar = do
  v <- get
  put (tail v)
  return (head v)

data StgState t = StgState
  { code  :: Expr  t
  , stack :: Stack t
  , heap  :: Map   t (Obj t)
  }
  
instance Show t => Show (StgState t) where
  show st@(StgState {code, stack, heap}) = 
    "heap: " ++ show heap ++ "code: " ++ show code

initialState :: [Function String] -> StgState String
initialState funs = StgState
  { code  = getMain funs
  , stack = []
  , heap  = initialHeap funs
  }

initialNames :: [String]
initialNames = map ("i." ++) $ [1..] >>= flip replicateM ['a'..'z']


getMain = getFunction "main"

getFunction :: Eq t  => t -> [Function t] -> Expr t
getFunction s ((Function name (OThunk c)):xs) | s == name = c
                                              | otherwise = getFunction s xs 

initialHeap :: Ord t => [Function t] -> Heap t
initialHeap = M.fromList . map (\(Function name obj) -> (name, obj))

step :: (Data t, Ord t, Eq t) => StgState t -> StgM t (Maybe (Rule, StgState t))
step st@(StgState {code, stack, heap}) = case code of
  ELet  b defs _ -> do
    vars <- replicateM (length defs) newVar
    let (ids, _objs) = unzip defs
        code'@(ELet _ defs' e') = substList ids vars code
        (_ids, objs) = unzip defs'
        heap' = foldr (\ (name, obj) h -> M.insert name obj h) 
                      heap
                      (zip vars objs)
    return . Just $
      (RLet, st { code = e'
                , heap = heap'
                })
  ECase e br     -> undefined
  ECall i args   -> undefined
  EAtom at       -> undefined
  _              -> undefined

eval :: [Function String] -> [(Rule, StgState String)]
eval funs = (RInitial, st) : evalState (go st) initialNames
  where
    st = initialState funs
    go st = do
      res <- step st
      case res of
        Nothing    -> return []
        Just (r, st') -> ((r, st') :) `fmap` go st'
      