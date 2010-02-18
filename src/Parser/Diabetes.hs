{-# LANGUAGE PackageImports #-}
module Parser.Diabetes where

import Parser.Pretty.Pretty
import Control.Applicative
import Control.Monad
import "mtl" Control.Monad.State
import Data.Either

import Data.Set (Set)
import qualified Data.Set as S

import qualified Parser.SugarTree as ST
import qualified Stg.AST          as AST

import Parser.SugarParser

type Dia a b = State (DiaState a) b 

data DiaState t = DiaState
    { nameSupply :: [t]
    , emptyCons  :: Set t
    , mkEmptyCon :: t -> t
    }

test :: String -> [AST.Function String]
test str = case parseSugar str of
  Right funs -> run funs
  Left err -> error (show err)

-- | Create a fresh unbound variable
newVar :: Dia a a 
newVar = do
  st@(DiaState { nameSupply = (n:ns) }) <- get
  put $ st { nameSupply = ns }
  return n

run :: [ST.Function String] -> [AST.Function String]
run fs = conses ++ funs
  where
    (funs, DiaState { emptyCons = set }) = runState (mapM desugar fs) defaultState
    conses = map (\c -> AST.Function (toCons c) (AST.OCon c [])) $ S.toList set
    toCons = ('$' :)
    defaultState = DiaState
        { nameSupply = map ("t." ++) $ [1..] >>= flip replicateM ['a'..'z']
        , emptyCons  = S.empty
        , mkEmptyCon = toCons
        }

{-
run :: ST.Function String -> AST.Function String
run fun = fst $ runState (desugar fun) $ DiaState
    { nameSupply = vars 
    , emptyCons  = []
    , mkEmptyCon = ('$' :)
    }
  where
    -- infinite list of var names
    vars = map ("t." ++) $ [1..] >>= flip replicateM ['a'..'z']
-}

desugar :: Ord a => ST.Function a -> Dia a (AST.Function a)
desugar (ST.Function t ts expr) = 
    liftM (AST.Function t) (createFun ts expr)

createFun :: Ord t => [t] -> ST.Expr t -> Dia t (AST.Obj t)
createFun args expr | null args = liftM AST.OThunk      (desugarE expr)
                    | otherwise = liftM (AST.OFun args) (desugarE expr)

desugarE :: Ord t => ST.Expr t -> Dia t (AST.Expr t)
desugarE (ST.EAtom t) = return $ AST.EAtom $ atomST2AST t
desugarE (ST.ECall t exprs) = do
  (atoms, binds) <- magic exprs
  case binds of
    []    -> return $ AST.ECall t atoms
    binds -> return $ AST.ELet False binds
                               (AST.ECall t atoms)
desugarE (ST.ECon t exprs) | null exprs = do
    st <- get
    put $ st { emptyCons = S.insert t (emptyCons st) }
    return (AST.EAtom (AST.AVar (mkEmptyCon st t)))
                           | otherwise  = do
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

desugarB :: Ord t => ST.Branch t -> Dia t (AST.Branch t)
desugarB (ST.BCon t binds exp) = liftM (AST.BCon t binds) (desugarE exp)
desugarB (ST.BDef t exp) = liftM (AST.BDef t) (desugarE exp)

magic :: Ord a => [ST.Expr a] -> Dia a ([AST.Atom a], [(a, AST.Obj a)])
magic [] = return ([], [])
magic ((ST.EAtom x):xs) = do
  (as,bs) <- magic xs
  return (atomST2AST x : as, bs)
magic (x:xs) = do
  var     <- newVar
  obj     <- AST.OThunk <$> desugarE x
  (as,bs) <- magic xs
  return ((AST.AVar var) : as, (var, obj) : bs)

-- | converts atoms from ST to AST
atomST2AST :: ST.Atom a -> AST.Atom a
atomST2AST (ST.AVar t) = AST.AVar t
atomST2AST (ST.ANum n) = AST.ANum n
