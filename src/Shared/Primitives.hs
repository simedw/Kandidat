{-# LANGUAGE Rank2Types, ViewPatterns, PackageImports, DeriveDataTypeable #-}
module Shared.Primitives 
    ( primitives
    , priorities
    , mathFunctions
    , opDesc
    , intOp
    , dblOp
    , intCmp
    , dblCmp
    , assoc
    , prio
    , toParsecAssoc
    , Primitive(..)
    , isUnary
    ) where

import Text.ParserCombinators.Parsec.Expr hiding (Assoc,Infix)
import Data.List
import Data.Function
import "syb" Data.Generics hiding (Infix)
import Stg.Variable

data Assoc = Infixr | Infixl | Infix
  deriving (Data,Typeable,Show,Eq,Ord)

toParsecAssoc (assoc -> Infixr) = AssocRight
toParsecAssoc (assoc -> Infixl) = AssocLeft
toParsecAssoc (assoc -> Infix)  = AssocNone

data Primitive t
  = IntOp  { opDesc :: t
           , intOp  :: Integer -> Integer -> Integer
           , assoc  :: Assoc
           , prio   :: Int
           }
  | DblOp  { opDesc :: t
           , dblOp  :: Double -> Double -> Double
           , assoc  :: Assoc
           , prio   :: Int
           }
  | IntCmp { opDesc :: t
           , intCmp :: Integer -> Integer -> Bool
           , assoc  :: Assoc
           , prio   :: Int
           }
  | DblCmp { opDesc :: t
           , dblCmp :: Double -> Double -> Bool
           , assoc  :: Assoc
           , prio   :: Int
           }
  | MathFun { opDesc :: t 
            , mathFun :: Double -> Double 
            }
  deriving (Data,Typeable)

instance Show t => Show (Primitive t) where
    show x = "prim(" ++ show (opDesc x) ++ ")"

instance Eq t => Eq (Primitive t)   where (==) = (==) `on` opDesc
instance Ord t => Ord (Primitive t) where compare = compare `on` opDesc

isUnary :: Primitive t -> Bool
isUnary (MathFun{}) = True
isUnary _           = False

mathFunctions :: [Primitive String]
mathFunctions = 
    [ MathFun "sin" sin
    , MathFun "cos" cos
    , MathFun "tan" tan
    , MathFun "atan" atan
    , MathFun "sqrt" sqrt
    , MathFun "log" log
    , MathFun "exp" exp
    ]

primitives :: [Primitive String]
primitives = 
       intDblOp  "+"  (+)  Infixl 6
    ++ intDblOp  "-"  (-)  Infixl 6
    ++ intDblOp  "*"  (*)  Infixl 7
    ++ [ DblOp   "/." (/)  Infixl 7 
       , IntOp   "/"  div  Infixl 7 
       , IntOp   "%"  mod  Infixl 7 ]
    ++ intDblCmp "<"  (<)  Infix 4
    ++ intDblCmp "<=" (<=) Infix 4
    ++ intDblCmp "==" (==) Infix 4
    ++ intDblCmp ">=" (>=) Infix 4
    ++ intDblCmp ">"  (>)  Infix 4
  where
    intDblOp :: String -> (forall a. Num a => a -> a -> a) 
             -> Assoc -> Int -> [Primitive String]
    intDblOp s op a p = [ IntOp s op a p , DblOp (s++".") op a p ]
   
    intDblCmp :: String -> (forall a. Ord a => a -> a -> Bool) 
              -> Assoc -> Int -> [Primitive String]
    intDblCmp s op a p = [ IntCmp s op a p , DblCmp (s++".") op a p ]
                        
priorities :: [[Primitive String]]
priorities = groupBy ((==) `on` prio) 
           $ sortBy (flip compare `on` prio) primitives




