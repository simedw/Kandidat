    {-# LANGUAGE PackageImports #-}
module Parser.Diabetes where

-- import Parser.Pretty.Pretty
import Control.Applicative
import Control.Monad
import Control.Arrow
import "mtl" Control.Monad.State
import Data.Either

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map as M

import qualified Parser.SugarTree as ST
import qualified Stg.AST          as AST
import qualified Stg.GC           as GC
import Stg.Variable

import Parser.SugarParser


type Dia a = State (DiaState a) 

data DiaState t = DiaState
    { nameSupply :: [t]
    , emptyCons  :: Set t
    , mkEmptyCon :: t -> t
    }



test :: String -> [AST.Function String]
test str = case parseSugar str of
  Right funs -> run [] funs
  Left err -> error (show err)

-- | Create a fresh unbound variable
newVar :: Dia a a 
newVar = do
  st@(DiaState { nameSupply = (n:ns) }) <- get
  put $ st { nameSupply = ns }
  return n

run :: [AST.Function String] -> [ST.Function String] -> [AST.Function String]
run prelude fs = conses ++ funs
  where
    (funs, DiaState { emptyCons = set }) = runState action defaultState
    conses = map (\c -> AST.Function (toCons c) (AST.OCon c [])) $ S.toList set
    toCons = ('$' :)
    action = do
        ds <- mapM desugar fs
        nullCon <- gets emptyCons
        let dt = S.fromList (map (\(AST.Function t _) -> t) (ds ++ prelude)) `S.union` nullCon
        lds <- mapM (llift dt) ds
        return $ prelude ++ concat lds
        
    defaultState = DiaState
        { nameSupply = map ("t." ++) $ [1..] >>= flip replicateM ['a'..'z']
        , emptyCons  = S.empty
        , mkEmptyCon = toCons
        }

desugar :: Variable a => ST.Function a -> Dia a (AST.Function a)
desugar (ST.Function t ts expr) = 
    liftM (AST.Function t) (createFun ts expr)

createFun :: Variable t => [t] -> ST.Expr t -> Dia t (AST.Obj t)
createFun args expr | null args = AST.OThunk [] 0 `liftM` desugarE expr
                    | otherwise = AST.OFun args 0 `liftM` desugarE expr

desugarE :: Variable t => ST.Expr t -> Dia t (AST.Expr t)
desugarE x@(ST.EAtom _) = do
    ([atom], binds) <- magic [x] -- hackity hackity hack!
    case binds of
        [] -> return $ AST.EAtom atom
        _  -> return $ AST.mkELet False binds (AST.EAtom atom)

desugarE (ST.ELam args expr) = do
    n <- newVar
    e <- desugarE expr
    return $ AST.mkELet False [(n,AST.OFun args 0 e)] (AST.EAtom (AST.AVar (AST.Heap n)))

desugarE (ST.ECall t exprs) = do
  (atoms, binds) <- magic exprs
  case binds of
    []    -> return $ AST.ECall (AST.Heap t) atoms
    binds -> return $ AST.mkELet False binds
                               (AST.ECall (AST.Heap t) atoms)

desugarE (ST.EOpt expr with) = do
    ([atom], binds) <- magic [expr] -- hackity hack
    dummy <- newVar
    varOpt <- newVar
    varThunk <- newVar
    (settings,sbinds) <- second concat <$> mapAndUnzipM desugarS with
    let opt = AST.OOpt atom settings
    -- Need to force optimising of all numbers, as in inline map (length xs),
    -- this is done via standard case-seq
    let thunk = AST.OThunk [] 0 $ foldr    
            (\atom e -> AST.ECase (AST.EAtom (AST.AVar $ AST.Heap atom)) [AST.BDef dummy e])
            (AST.EAtom (AST.AVar $ AST.Heap varOpt))
            (map fst sbinds)
    return $ AST.mkELet False 
                        (binds ++ sbinds ++ [(varOpt, opt),(varThunk, thunk)])
                        (AST.EAtom (AST.AVar (AST.Heap varThunk)))
  where
    desugarS :: Variable t => ST.Setting t -> Dia t (AST.Setting t, [(t, AST.Obj t)])
    desugarS (ST.Inlinings expr) = do
        ([atom],binds) <- magic [expr]
        return (AST.Inlinings atom,binds)
    desugarS (ST.Inline f expr)  = do
        ([atom],binds) <- magic [expr]
        return (AST.Inline f atom,binds)
    desugarS ST.CaseBranches  = return (AST.CaseBranches, [])
 
desugarE (ST.ECon t exprs) | null exprs = do
    st <- get
    put $ st { emptyCons = S.insert t (emptyCons st) }
    return (AST.EAtom (AST.AVar (AST.Heap (mkEmptyCon st t))))
                           | otherwise  = do
    (atoms, binds) <- magic exprs
    var <- newVar
    let con = AST.OCon t atoms
    return $  AST.mkELet False (binds ++ [(var, con)])
                             (AST.EAtom . AST.AVar . AST.Heap $ var)

desugarE (ST.ELet rec vars expr) = do
  binds <- mapM (\(name, args, exp) 
                   -> (,) name `liftM` createFun args exp) vars
  AST.mkELet rec binds `liftM` desugarE expr

desugarE (ST.ECase expr branches) = 
  liftM2 AST.ECase (desugarE expr) (mapM desugarB branches)


desugarB :: Variable t => ST.Branch t -> Dia t (AST.Branch t)
desugarB (ST.BCon t binds exp) = liftM (AST.BCon t binds) (desugarE exp)
desugarB (ST.BDef t exp) = liftM (AST.BDef t) (desugarE exp)

magic :: Variable a => [ST.Expr a] -> Dia a ([AST.Atom a], [(a, AST.Obj a)])
magic [] = return ([], [])
magic (ST.EAtom (ST.AVar var):xs) = do
    (as,bs) <- magic xs
    return (AST.AVar (AST.Heap var) : as, bs)

magic (ST.EAtom (ST.AStr str):xs) = do
   (as,bs) <- magic xs
   var     <- newVar
   n       <- newVar
   vars    <- replicateM ((length str) * 2) newVar
   let  varNums    = zip vars $ reverse str
        conVars    = drop (length str) vars
        conVarNums = zip conVars varNums
        rest       = ((n, AST.OCon nilCon []) : lets conVarNums n)
   return (AST.AVar (AST.Heap var) : as,
          (var, AST.OThunk [] 0 (AST.mkELet False rest 
            (AST.EAtom . AST.AVar . AST.Heap $ fst $ last conVarNums))) : bs)
  where
   lets [] _  = []
   lets ((conVar, (numVar, s)):rest) prev
               = (numVar, AST.OCon chrCon [AST.AChr s])
               : (conVar, AST.OCon consCon [AST.AVar (AST.Heap numVar), AST.AVar (AST.Heap prev)])
               : lets rest conVar
        

magic (ST.EAtom x:xs) = do -- x is either ANum or ADec  or AChr or AStr
    (as,bs) <- magic xs
    var <- newVar
    let c = case x of
            ST.ANum n -> numCon
            ST.ADec d -> decCon
            ST.AChr c -> chrCon
    return (AST.AVar (AST.Heap var) : as, (var, AST.OCon c [atomST2AST x]) : bs)
magic (ST.ECon t [] : xs ) = do
    (as, bs) <- magic xs
    mkCon <- gets mkEmptyCon
    modify $ \s -> s { emptyCons = S.insert (mkCon t) (emptyCons s) } 
    return (AST.AVar (AST.Heap $ mkCon t) : as, bs) 
magic (x:xs) = do
    var     <- newVar
    obj     <- AST.OThunk [] 0 <$> desugarE x
    (as,bs) <- magic xs
    return ((AST.AVar $ AST.Heap var) : as, (var, obj) : bs)

-- | converts non-var atoms from ST to AST
atomST2AST :: ST.Atom a -> AST.Atom a
atomST2AST (ST.ANum n) = AST.ANum n
atomST2AST (ST.ADec n) = AST.ADec n
atomST2AST (ST.AChr n) = AST.AChr n

call :: AST.Var a -> [AST.Atom a] -> AST.Expr a
call f [] = AST.EAtom $ AST.AVar f
call f as = AST.ECall f as

llift :: Variable a => Set a -> AST.Function a -> Dia a [AST.Function a]
llift dt (AST.Function t (AST.OFun args size e)) = do 
    (e', fs) <- lliftExpr dt e
    return (AST.Function t (AST.OFun args size e') : fs)
llift dt (AST.Function t obj) = do 
    (obj', fs) <- lliftObj dt obj
    return (AST.Function t obj' : fs)

lliftObj :: Variable a => Set a -> AST.Obj a -> Dia a (AST.Obj a, [AST.Function a])
lliftObj dt obj = case obj of
    AST.OFun args s e -> do
        funName <- newVar
        (e', fs) <- lliftExpr dt e
        let fv = S.toList $ GC.freeVars obj `S.difference` dt
        return ( AST.OThunk [] 0 (call (AST.Heap funName) (map (AST.AVar . AST.Heap) fv))
               , AST.Function funName (AST.OFun (fv ++ args) s e') : fs)
    AST.OPap t as -> return (obj, [])
    AST.OCon t as -> return (obj, [])
    AST.OThunk _ _ expr -> do
        (e', fs) <- lliftExpr dt expr
        return (AST.OThunk [] 0 e', fs)
    AST.OBlackhole -> return (obj, [])
    AST.OOpt _ _   -> return (obj, [])

lliftExpr :: Variable a => Set a -> AST.Expr a -> Dia a (AST.Expr a, [AST.Function a])
lliftExpr dt expr = case expr of
    AST.EAtom _a -> do
        return (expr, [])
    AST.ECall _t _as -> do
        return (expr, [])
    AST.EPop _pop _as -> return (expr, [])
    AST.ELet binds e -> do
        (binds', fs) <- lliftBind dt binds
        (e', efs) <- lliftExpr dt e
        return (AST.ELet binds' e', efs ++ fs)
    AST.ECase escrut brs -> do
        (escrut', efs) <- lliftExpr dt escrut
        (brs', bfs) <- liftM unzip (mapM (lliftBranch dt) brs)
        return (AST.ECase escrut' brs', efs ++ concat bfs)
    AST.ESVal _sval -> return (expr, [])
            
lliftBind :: Variable a => Set a -> AST.Bind a -> Dia a (AST.Bind a, [AST.Function a])
lliftBind dt (AST.NonRec t obj) = do
    (obj', fs) <- lliftObj dt obj
    return (AST.NonRec t obj', fs)
lliftBind dt (AST.Rec binds) = do
    (binds', fss) <- liftM unzip (mapM recBind binds)
    return (AST.Rec binds', concat fss)
  where
    recBind (t, obj) = do
    (obj', ofs) <- lliftObj dt obj
    return ((t, obj'), ofs)

lliftBranch :: Variable a => Set a -> AST.Branch a -> Dia a (AST.Branch a, [AST.Function a])
lliftBranch dt branch = case branch of
    AST.BCon con args e -> do
        (e', fs) <- lliftExpr dt e
        return (AST.BCon con args e', fs)
    AST.BDef var e -> do
        (e', fs) <- lliftExpr dt e
        return (AST.BDef var e', fs)
