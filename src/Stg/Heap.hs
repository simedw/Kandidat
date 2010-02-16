module Stg.Heap where

import Data.Map(Map)

type Heap  t = Map t (Obj t)
