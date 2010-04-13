{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PackageImports #-}
module Stg.Variable where

import Control.Monad (replicateM)

import "syb" Data.Generics
import Data.Generics.PlateData

class (Show t, Eq t, Ord t, Data t, Typeable t) => Variable t where 
    namesupply   :: [t]
    mainFunction :: t
    mkcons       :: t -> t 
    trueCon, falseCon, nilCon, consCon, getIntList, getInt , numCon, decCon, chrCon :: t

instance Variable String where
    namesupply   = map ("i." ++) $ [1..] >>= flip replicateM ['a'..'z']
    mainFunction = "main"
    mkcons       = ('$':)
    trueCon      = "True"
    falseCon     = "False"
    nilCon       = "Nil"
    consCon      = "Cons"
    getIntList   = "getIntList"
    getInt       = "getInt"
    numCon       = "I#"
    decCon       = "D#"
    chrCon       = "C#"
    
-- A simple prototype. Expect the unexpected.
instance Variable Int where
    namesupply   = map negate [2..]
    mainFunction = -1
    mkcons       = (maxBound -)   -- uuh?
    trueCon      = 19
    falseCon     = 20
    
boolToCon :: Variable t => Bool -> t
boolToCon True  = mkcons  trueCon
boolToCon False = mkcons  falseCon