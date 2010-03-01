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
import qualified Stg.PrePrelude   as PP

import Parser.SugarParser


type Dia a = State (DiaState a) 

data DiaState t = DiaState
    { nameSupply :: [t]
    , emptyCons  :: Set t
    , mkEmptyCon :: t -> t
    , numCon :: t
    , decCon :: t
    , chrCon :: t
    , consCon :: t
    , nilCon :: t
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
        , numCon     = PP.numCon
        , decCon     = PP.decCon
        , chrCon     = PP.chrCon
        , consCon    = PP.consCon
        , nilCon     = PP.nilCon
        }

desugar :: Ord a => ST.Function a -> Dia a (AST.Function a)
desugar (ST.Function t ts expr) = 
    liftM (AST.Function t) (createFun ts expr)

createFun :: Ord t => [t] -> ST.Expr t -> Dia t (AST.Obj t)
createFun args expr | null args = liftM AST.OThunk      (desugarE expr)
                    | otherwise = liftM (AST.OFun args) (desugarE expr)

desugarE :: Ord t => ST.Expr t -> Dia t (AST.Expr t)
desugarE x@(ST.EAtom _) = do
    ([atom], binds) <- magic [x] -- hackity hackity hack!
    case binds of
        [] -> return $ AST.EAtom atom
        _  -> return $ AST.ELet False binds (AST.EAtom atom)
desugarE (ST.ELam args expr) = do
    n <- newVar
    e <- desugarE expr
    return $ AST.ELet False [(n,AST.OFun args e)] (AST.EAtom (AST.AVar n))
desugarE (ST.ECall t exprs) = do
  (atoms, binds) <- magic exprs
  case binds of
    []    -> return $ AST.ECall t atoms
    binds -> return $ AST.ELet False binds
                               (AST.ECall t atoms)
desugarE (ST.EOpt expr) = do
    ([atom], binds) <- magic [expr] -- hackity hack
    var <- newVar
    let opt = AST.OOpt atom
    return $ AST.ELet False (binds ++ [(var, opt)])
                            (AST.EAtom (AST.AVar var))
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
                   -> (,) name `liftM` createFun args exp) vars
  AST.ELet rec binds `liftM` desugarE expr

desugarE (ST.ECase expr branches) = 
  liftM2 AST.ECase (desugarE expr) (mapM desugarB branches)

desugarB :: Ord t => ST.Branch t -> Dia t (AST.Branch t)
desugarB (ST.BCon t binds exp) = liftM (AST.BCon t binds) (desugarE exp)
desugarB (ST.BDef t exp) = liftM (AST.BDef t) (desugarE exp)

magic :: Ord a => [ST.Expr a] -> Dia a ([AST.Atom a], [(a, AST.Obj a)])
magic [] = return ([], [])
magic (ST.EAtom (ST.AVar var):xs) = do
    (as,bs) <- magic xs
    return (AST.AVar var : as, bs)

magic (ST.EAtom (ST.AStr str):xs) = do
   (as,bs) <- magic xs
   var     <- newVar
   n       <- newVar
   vars    <- replicateM ((length str) * 2) newVar
   consC   <- gets consCon
   nilC    <- gets nilCon
   chrC    <- gets chrCon
   let  varNums    = zip vars $ reverse str
        conVars    = drop (length str) vars
        conVarNums = zip conVars varNums
        rest       = ((n, AST.OCon nilC []) : lets consC chrC conVarNums n)
   return (AST.AVar var : as,
          (var, AST.OThunk (AST.ELet False rest 
            (AST.EAtom . AST.AVar . fst $ last conVarNums))) : bs)
  where
   lets _ _ [] _  = []
   lets consC chrC ((conVar, (numVar, s)):rest) prev
               = (numVar, AST.OCon chrC [AST.AChr s])
               : (conVar, AST.OCon consC [AST.AVar numVar, AST.AVar prev])
               : lets consC chrC rest conVar
        

magic (ST.EAtom x:xs) = do -- x is either ANum or ADec  or AChr or AStr
    (as,bs) <- magic xs
    var <- newVar
    let cons = case x of
            ST.ANum n -> numCon
            ST.ADec d -> decCon
            ST.AChr c -> chrCon
    c <- gets cons
    return (AST.AVar var : as, (var, AST.OCon c [atomST2AST x]) : bs)
magic (ST.ECon t [] : xs ) = do
    (as, bs) <- magic xs
    mkCon <- gets mkEmptyCon
    return (AST.AVar (mkCon t) : as, bs) 
magic (x:xs) = do
    var     <- newVar
    obj     <- AST.OThunk <$> desugarE x
    (as,bs) <- magic xs
    return ((AST.AVar var) : as, (var, obj) : bs)

-- | converts non-var atoms from ST to AST
atomST2AST :: ST.Atom a -> AST.Atom a
atomST2AST (ST.ANum n) = AST.ANum n
atomST2AST (ST.ADec n) = AST.ADec n
atomST2AST (ST.AChr n) = AST.AChr n
