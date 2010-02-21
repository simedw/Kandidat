module Stg.Optimise where

import Control.Monad

import Stg.AST
import Stg.Types


optimise :: Expr t -> Heap t -> StgM t (Expr t, Heap t)
optimise e h = return $ (,) e h
