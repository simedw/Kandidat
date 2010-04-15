{-# LANGUAGE PackageImports #-}

module Parser.Locals where

--import Control.Applicative
import Control.Monad
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S 

import Stg.AST
import qualified Parser.SugarTree as ST
import qualified Parser.Diabetes as Diabetes
import Parser.SugarParser
import Stg.GC
import Stg.Variable
import Parser.Pretty.Pretty

type Local t = StateT Int (Reader (Map t Int))

test :: String -> String
test str = case parseSugar str of
    Right funs -> show $ map prFun $ localise $ Diabetes.run [] funs
    Left err   -> error (show err)

run :: Variable t => Local t (a t) -> a t
run = flip runReader M.empty . flip evalStateT 0

(<$>) :: Monad m => (a -> b) -> m a -> m b
(<$>) = liftM

(<*>) :: Monad m => m (a -> b) -> m a -> m b
(<*>) = ap

addLocal :: Variable t => t -> Local t a -> Local t (a, Int)
addLocal v l = local (\s -> 
    case M.lookup v s of
        Nothing -> M.insert v (M.size s) s 
        Just r  -> s) (do r <- l; i <- asks M.size; modify (max i); return (r, i))

addLocals :: Variable t => [t] -> Local t a -> Local t (a, Int)
addLocals (v:vs) l = fst <$> addLocal v (addLocals vs l)
addLocals []     l = do r <- l; i <- asks M.size; return (r, i)

getName :: Variable t => Var t -> t
getName (Heap n)    = n
getName (Local _ n) = n

getNameAtom :: Variable t => Atom t -> t
getNameAtom (AVar v) = getName v
getNameAtom x        = error $ "getNameAtom run on a non-var: " ++ show x

localMax :: Variable t => (Int -> Int) -> Local t a -> Local t a
localMax f l = do
    m <- get
    put (f m)
    r <- l
    put m
    return r

resetMax :: Variable t => Local t ()
resetMax = put 0

localise :: Variable t => [Function t] -> [Function t]
localise = map (run . (\(Function v o) -> Function v <$> localiseO o))

lookupLocal :: Variable t => t -> Local t (Var t)
lookupLocal v = do
    s <- ask
    case M.lookup v s of
        Nothing -> return $ Heap v
        Just i  -> return $ Local i v

localiseO :: Variable t => Obj t -> Local t (Obj t)
localiseO o = case o of
    OFun vs _ e   -> do
        (e',s) <- addLocals vs $ localiseE e
        s' <- get
        return $ OFun vs s' e'
    OPap v as   -> OPap v <$> mapM localiseA as
    OCon c as   -> OCon c <$> mapM localiseA as 
    OThunk fv _ e -> do
--        e' <- localiseE e
--        i  <- gets index
        localMax (const 0) $ do
            let names = map getNameAtom fv
            vars <- mapM lookupLocal (names ++ S.toList (freeVars e S.\\ S.fromList names))
            let list = filter isLocal vars
            exp <- local (const M.empty) $ fst <$> addLocals (map getName list) (localiseE e)
            s <- get
            return $ OThunk (map AVar list) s exp
                -- OThunk (map AVar list) (length list) <$> local (const M.empty) (fst <$> addLocals (map getName list) (localiseE e)) 
--        return $ OThunk (map (AVar . Heap) $ toList $ freeVars e) i e'
        -- kolla upp freevars, om de ar lokala ska de sparas. Annars ska de inte med
        -- den listan ar den nya env som vi ska kora exp pa
    OBlackhole  -> return OBlackhole
    OOpt a sets -> OOpt <$> localiseA a <*> mapM localiseSetting sets
    
localiseE :: Variable t => Expr t -> Local t (Expr t) 
localiseE e = case e of
    EAtom atom     -> EAtom <$> localiseA atom
    ECall f atoms  -> ECall <$> localiseV f <*> mapM localiseA atoms
    EPop pop atoms -> EPop pop <$> mapM localiseA atoms
    ELet bind expr -> case bind of
        NonRec t obj -> do (bi, ex) <- fst <$> addLocal t (liftM2 (,) (localiseO obj) (localiseE expr))
                           return $ ELet (NonRec t bi) ex
        Rec pars ->  do
            let (vs, os) = unzip pars
            (bi, ex) <- fst <$> addLocals vs (do
                os'  <- mapM (dependOn vs) os
                liftM2 (,) (mapM localiseO os') (localiseE expr))
            return $ ELet (Rec (zip vs bi)) ex

    ECase expr brs -> ECase <$> localiseE expr <*> mapM localiseBranch brs
    ESVal s        -> return $ ESVal s -- under the invariant that Atom is not a var
  where
    dependOn vs (OThunk _ c e) = do
        vs' <- mapM lookupLocal vs
        return $ OThunk (map (AVar) vs') c e
    dependOn _  x              = error $ "DependOn in Localise got: " ++ show x

localiseA :: Variable t => Atom t -> Local t (Atom t)
localiseA a = case a of
    AVar (Heap v) -> AVar <$> lookupLocal v
    _             -> return a


localiseV :: Variable t => Var t -> Local t (Var t)
localiseV v = case v of
    Heap t -> lookupLocal t
    _      -> return v 



{-
localiseBind :: Variable t => Bind t -> Local t (Bind t)
localiseBind b = case b of
    NonRec t obj -> NonRec t <$> (fst <$> addLocal t (localiseO obj))
    Rec pars ->  do
        let (vs, os) = unzip pars
        Rec <$> (zip vs <$> fst <$> addLocals vs (mapM localiseO os))
-}

localiseBranch :: Variable t  => Branch t -> Local t (Branch t)
localiseBranch br = case br of
    BCon t ts exp -> BCon t ts <$> fst <$> addLocals ts (localiseE exp)
    BDef t exp    -> BDef t <$> fst <$> addLocal t (localiseE exp)
  -- gor some i othunk fallet

localiseSetting :: Variable t => Setting t -> Local t (Setting t) 
localiseSetting s = case s of
    Inlinings a -> Inlinings <$> localiseA a
    Inline v a  -> Inline v  <$> localiseA a
    _           -> return s
{-


data Expr t   = EAtom (Atom t)
              | ECall (Var t) [Atom t]
              | EPop (Pop t) [Atom t]
              | ELet (Bind t) (Expr t)
              | ECase (Expr t) [Branch t]
              | ESVal (SValue t)

-}
