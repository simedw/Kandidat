module Stg.Branch where

import Data.Generics
import Data.Generics.PlateData

import Data.Maybe

import Stg.AST
import Stg.Substitution
import Stg.Types



instantiateBranch :: (Data t, Eq t) => t -> [Atom t] -> [Branch t] -> Maybe (Expr t)
instantiateBranch x atoms (BCon t ts e : bs) 
    | x == t    = Just $ substList ts atoms e
    | otherwise = instantiateBranch x atoms bs
instantiateBranch _ _ _ = Nothing

findDefaultBranch :: (Data t, Eq t) => Atom t -> [Branch t] -> Maybe (Expr t)
findDefaultBranch atom branches = listToMaybe [subst t atom e | BDef t e <- branches]

