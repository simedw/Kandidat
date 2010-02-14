{-# LANGUAGE PackageImports #-}
module Parser.Diabetes where

import Parser.Pretty.Pretty
import Control.Monad
import "mtl" Control.Monad.State
import Data.Either

import qualified Parser.SugarTree as ST
import qualified Stg.AST          as AST

import Parser.SugarParser

type Dia a b = State [a] b 

test :: String -> [AST.Function String]
test str = case parseSugar str of
  Right funs -> map run funs
  Left err -> error (show err)

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
    vars = map ("t." ++) $ [1..] >>= flip replicateM ['a'..'z']

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
desugarE (ST.ECall t exprs) = do
  (atoms, binds) <- magic exprs
  case binds of
    []    -> return $ AST.ECall t atoms
    binds -> return $ AST.ELet False binds
                               (AST.ECall t atoms)
desugarE (ST.ECon t exprs) = do
  (atoms, binds) <- magic exprs
  var <- newVar
  let con = AST.OCon t atoms
  return $  AST.ELet False (binds ++ [(var, con)])
                           (AST.EAtom (AST.AVar var))

desugarE (ST.ELet rec vars expr) = do
  binds <- mapM (\(name, args, exp) 
                    -> createFun args exp >>= return . (,) name) vars
  AST.ELet rec binds `liftM` desugarE expr

desugarE (ST.ECase expr branches) = 
  liftM2 AST.ECase (desugarE expr) (mapM desugarB branches)

desugarB :: ST.Branch t -> Dia t (AST.Branch t)
desugarB (ST.BCon t binds exp) = liftM (AST.BCon t binds) (desugarE exp)
desugarB (ST.BDef t exp) = liftM (AST.BDef t) (desugarE exp)

magic :: [ST.Expr a] -> Dia a ([AST.Atom a], [(a, AST.Obj a)])
magic [] = return ([], [])
magic ((ST.EAtom x):xs) = do
  (as,bs) <- magic xs
  return (atomST2AST x : as, bs)
magic (x:xs) = do
  var     <- newVar
  obj     <- object x
  (as,bs) <- magic xs
  return ((AST.AVar var) : as, (var, obj) : bs)


object :: ST.Expr a -> Dia a (AST.Obj a)
object x = liftM AST.OThunk (desugarE x)

-- | converts atoms from ST to AST
atomST2AST :: ST.Atom a -> AST.Atom a
atomST2AST (ST.AVar t) = AST.AVar t
atomST2AST (ST.ANum n) = AST.ANum n


