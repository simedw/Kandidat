module Stg.Heap where


import qualified Data.Map as M

import Util.Util
import Prelude hiding (lookup)

import Stg.AST

type Heap  t = M.Map t (HeapObject t) 

type HeapObject t = (Obj t,Location)

data Location = OnHeap | OnAbyss
  deriving (Show, Eq)

fromList :: Ord t => [(t,Obj t)] -> Heap t
fromList = M.fromList . map (\(x,o) -> (x,(o,OnHeap))) 

toList = M.toList

lookup :: Ord t => t -> Heap t -> Maybe (Obj t)
lookup = lookupHeap

insert :: Ord t => t -> Obj t -> Heap t -> Heap t
insert = insertHeap

locatedLookup :: Ord t => t -> Heap t -> Maybe (HeapObject t)
locatedLookup = M.lookup 

lookupAnywhere :: Ord t => t -> Heap t -> Maybe (Obj t)
lookupAnywhere = fmap fst `dot` M.lookup

lookupHeap :: Ord t => t -> Heap t -> Maybe (Obj t) 
lookupHeap x h = case M.lookup x h of
    Just (o,OnHeap) -> Just o
    Just (_,OnAbyss)     -> Nothing -- error "Was on abyss!"
    _               -> Nothing -- 

lookupAbyss :: Ord t => t -> Heap t -> Maybe (Obj t)
lookupAbyss x h = case M.lookup x h of
    Just (o,OnAbyss) -> Just o
    _           -> Nothing

insertHeap :: Ord t => t -> Obj t -> Heap t -> Heap t
insertHeap t o = M.insert t (o,OnHeap) 

insertAbyss :: Ord t => t -> Obj t -> Heap t -> Heap t
insertAbyss t o = M.insert t (o,OnAbyss)
