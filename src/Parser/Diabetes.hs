{-# LANGUAGE PackageImports #-}
module Diabetes where

import Parser.Pretty.Pretty
import Control.Monad
import "mtl" Control.Monad.State
import Data.Either

import qualified Parser.SugarTree as ST
import qualified Stg.AST          as AST

type Dia a b = State [a] b 

newVar :: Dia a a 
newVar = do
 v <- get
 put (tail v)
 return (head v)

-- NOTE: We need to add a pass to remove empty lets!

run :: ST.Function String -> AST.Function String
run fun = fst $ flip runState vars $ desugar fun
  where
    -- infinite list of var names
    vars = [1..] >>= flip replicateM ['a'..'z']

desugar :: ST.Function a -> Dia a (AST.Function a)
desugar (ST.Function t [] expr) = 
    liftM (AST.Function t) (object expr)
desugar (ST.Function t ts expr) = 
    liftM (AST.Function t) (createFun ts expr)

createFun :: [t] -> ST.Expr t -> Dia t (AST.Obj t)
createFun args expr = liftM (AST.OFun args)
                            (desugarE expr)

desugarE :: ST.Expr t -> Dia t (AST.Expr t)
desugarE (ST.EAtom t) = return $ AST.EAtom $ atomST2AST t
-- function calls, no arguments
desugarE (ST.ECall t [])    = do
  return $ AST.ECall t []
-- function calls with arguments
desugarE (ST.ECall t exprs) = do
 res <- magic exprs
 return $  AST.ELet False (lefts res)
                          (AST.ECall t (map convert res))
  where
    -- Left if Expr is not atomic
    magic :: [ST.Expr a] -> Dia a [Either (a, AST.Obj a)
                                          (AST.Atom a)]
    magic [] = return []
    magic ((ST.EAtom x):xs) = liftM ((:) (Right $ atomST2AST x))
                                         (magic xs)
    magic (x:xs) = do
      var    <- newVar
      obj    <- object x
      rest   <- magic xs
      return $ Left (var, obj) : rest
    -- convert no atomics to vars
    convert :: Either (a, AST.Obj a) (AST.Atom a) -> AST.Atom a
    convert (Left (x,_)) = AST.AVar x
    convert (Right x   ) = x

object :: ST.Expr a -> Dia a (AST.Obj a)
object x = liftM AST.OThunk (desugarE x)

-- | converts atoms from ST to AST
atomST2AST :: ST.Atom a -> AST.Atom a
atomST2AST (ST.AVar t) = AST.AVar t
atomST2AST (ST.ANum n) = AST.ANum n


