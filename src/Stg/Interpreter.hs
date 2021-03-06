{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Stg.Interpreter where

-- Naive Stg interpreter

import Control.Monad
import "mtl" Control.Monad.State

import Data.Char
import "syb" Data.Generics
import Data.Maybe

{-
import Data.Map(Map)
import qualified Data.Map as M
-}

import Stg.AST
import Stg.GC
import Stg.Input
import Stg.Variable
import Stg.Stack
import Stg.Optimise
import Stg.Rules
import Stg.Substitution
import Stg.Types
import Stg.Branch
import Stg.Heap (Heap, Location(..))
import Shared.Primitives
import qualified Stg.Heap as H
 
topCase :: ContStack t -> Bool
topCase (CtCase _ : _) = True
topCase _              = False  

topArg :: ContStack t -> Bool
topArg (CtArg _ : _) = True
topArg _             = False

topUpd :: ContStack t -> Bool
topUpd (CtUpd _ : _) = True
topUpd (CtOUpd _ : _) = True
topUpd _             = False

topOpt :: ContStack t -> Bool
topOpt (CtOpt _ : _) = True
topOpt _             = False

topPrint :: ContStack t -> Bool
topPrint (CtPrint : _) = True
topPrint _             = False

topPrintCon :: ContStack t -> Bool
topPrintCon (CtPrintCon _ _ _ : _) = True
topPrintCon _                      = False

topIsO :: ContStack t -> Bool
topIsO (cont : _) = case cont of
    CtOFun  _ _ _   -> True
    CtOCase _       -> True
    CtOLet _        -> True
    CtOBranch{}     -> True
    CtOApp _ _      -> True
    _               -> False
topIsO _          = False

topOInstant :: ContStack t -> Bool
topOInstant (CtOInstant n: _) = n <= 0
topOInstant _                = False

decTop :: ContStack t -> ContStack t
decTop (CtOInstant n : xs) = CtOInstant (n-1) : xs
decTop xs = xs 

numArgs :: ContStack t -> Int
numArgs = length . takeWhile isArg
  where 
    isArg (CtArg _) = True
    isArg _         = False
  
unArg :: Show t => Cont t -> Atom t
unArg (CtArg a) = a
unArg o = error $ "unArg: not an arg: " ++ show o 


initialState :: Variable t => [Function t] -> StgState t
initialState funs = StgState
  { code     = mainF
  , cstack   = [CtPrint]
  , astack   = newFrame []
  , heap     = initialHeap funs
  , settings = []
  } where
        (mainF, size) = getMain funs

--initialNames :: [String]
--initialNames = map ("i." ++) $ [1..] >>= flip replicateM ['a'..'z']

initialStgMState :: Variable t => StgMState t
initialStgMState = StgMState
    { nameSupply = namesupply
    , mkCons     = mkcons            --- is this really needed?
    }

getMain :: Variable t => [Function t] -> (Expr t, Int)
getMain = getFunction mainFunction

getFunction :: Variable t => t -> [Function t] -> (Expr t, Int)
getFunction s ((Function name o):xs) | s == name = let OThunk [] size c = o in (c, size)
                                     | otherwise = getFunction s xs 
getFunction s [] = error $ "No \"" ++ show s  ++ "\" function"

initialHeap :: Variable t => [Function t] -> Heap t
initialHeap = H.fromList . map (\(Function name obj) -> (name, obj))


step :: Variable t => StgState t -> StgM t (Maybe (Rule, StgState t))

step st@(OmegaState {..}) = omega' cstack astack heap code settings
step st@(IrrState {..})   = irr' cstack astack heap code settings
step st@(PsiState {..})   = case code of
    EAtom code -> psi' cstack astack heap code letBinds settings

step st@(StgState {..})   = step' $ st {cstack = decTop cstack}
  where
    step' st'@(StgState {..}) = case code of
       _ | topOInstant cstack -> omega (ROmega "from machine after OInstant") 
                                        (drop 1 cstack) astack heap code settings -- for inlining
     
       ECase expr branch     -> case expr of
           EAtom a -> case lookupAtom a of
               a'@(AVar (Heap var)) -> case H.lookup var heap of
                   Nothing             -> error "var not in heap (maybe on abyss)" -- return Nothing
                   Just (OThunk _ _ _)   -> rcase expr branch
                   Just (OCon c atoms) -> case instantiateBranch c atoms branch of
                       Nothing   -> rany expr a' branch
                       Just expr -> -- lagg till atoms pa arg-stacken
                                    rcasecon st atoms expr 
                   _             -> rany expr a' branch
               _ -> rany expr a branch
           _ -> rcase expr branch
                      
       ELet defs exp  -> rlet st defs exp
       EPop  op args     -> rprimop st op args 
       ECall ident args  -> rpush st ident args 
       EAtom a
            -- Top is a I# 2, but we should save it in a thunk 
            | not (isVar a) && topUpd cstack   -> rupdate st (OThunk [] 0 code)
            | not (isVar (lookupAtom a)) && topCase cstack  -> rret st
            | not (isVar (lookupAtom a)) && topPrint cstack -> rprintval st a
            | not (isVar (lookupAtom a)) && topIsO cstack 
                -> psi (RPsi "Machine found atom and can continue to optimise")
                       cstack (popFrame astack) heap (lookupAtom a) [] settings
       EAtom a -> case lookupAtom a of
         (AVar (Heap var)) -> case H.lookup var heap of  
           Nothing  -> return Nothing
           Just obj -> case obj of
               OThunk fv size expr       -> rthunk st (map lookupAtom fv) size expr var
               _ | topIsO cstack  
                    -> psi (RPsi "machine found non-thunk, continue optimise")
                            cstack (popFrame astack) heap (lookupAtom a) [] settings
               OOpt alpha sets   -> roptimise st alpha sets var
               _ | topUpd  cstack -> rupdate st obj
               _ | topCase cstack -> rret st
--               _ | topContOpt stack -> rcontopt st
               OCon t atoms | topPrint cstack -> rprintcon st t atoms
               _            | topPrintCon cstack -> rprintfun st
               OPap ident atoms | topArg cstack -> rpenter st ident atoms
                                | topOpt cstack -> roptpap st ident atoms
               OFun args size expr
                                | topOpt cstack -> roptfun st args size expr
                                | otherwise    -> 
                   let lenArgs = length args
                       stackArgs = numArgs cstack 
                   -- depending on the number of arguments the function is either
                   -- saturated or not 
                   in case lenArgs <= stackArgs of
                       True  -> rfenter st args lenArgs size expr
                       False -> rpap st stackArgs var 
               _ | topOpt cstack  -> rupdateopt st obj (Heap var)
               _                               -> return Nothing
         s -> error $ show s
       ESVal sval | topPrintCon cstack -> rprintcont st sval
       -- if there is no rule to apply, do nothing
       _              -> return Nothing
     where
      lookupAtom (AVar v) = case v of
        Heap x    -> AVar v
        Local x _ -> lookupStackFrame x astack
                    -- det har ar farligt sager daniel
                    -- for det kan vara i en annan frame
      lookupAtom (AUnknown i t) = lookupStackFrame i astack
      lookupAtom a              = a
      allocObj o = case o of
        OCon c atoms    -> OCon c (map lookupAtom atoms)
        OThunk fv s exp -> OThunk (map lookupAtom fv) s exp
        OPap   t atoms  -> OPap t (map lookupAtom atoms)
        OOpt alpha set  -> OOpt (lookupAtom alpha) set
        _               -> o
      
      st = st' { cstack = cstack }
      
      {-
      rlet st@(StgState {..}) defs = do
          vars <- replicateM (length (getBinds defs)) newVar
          let ids  = map fst (getBinds defs)
              code'@(ELet defs' e') = substList ids (map AVar vars) code
              objs = map snd $ (getBinds defs')
              heap' = foldr (\(v,o) h -> H.insert v o h) heap (zip vars objs)
          returnJust (RLet, st { code = e'
                               , heap = heap' })
      -}
      rlet st@(StgState {..}) (NonRec t obj) e = do
          var <- newVar 
          let astack' = pushArgs [AVar $ Heap var] astack
              heap'   = H.insert var (allocObj obj) heap
          returnJust (RLet, st { code = e
                               , heap = heap' 
                               , astack = astack'})
            
      rlet st@(StgState {..}) (Rec list) e = do
          vars <- replicateM (length list) newVar 
          let astack' = pushArgs (map (AVar . Heap) vars) astack
              objs    = map (allocObjRec vars . snd) list
              heap'   = foldr (\(var,obj) heap -> H.insert var obj heap) heap (zip vars objs) 
          returnJust (RLet, st { code = e
                               , heap = heap' 
                               , astack = astack'})         
        where
            allocObjRec v o = case o of
                OThunk fv s exp -> OThunk (map (AVar . Heap) v 
                                   ++ map lookupAtom (drop (length v) fv)) s exp 
                _               -> o
                            
      -- fixed
      rcase expr branch = returnJust 
            (RCaseCon
            , st { code   = expr
                 , cstack = CtCase branch : cstack
                 , astack = duplicateFrame astack
                 }
            )
      -- fixed
      rany (EAtom var) defvar branch = case findDefaultBranch (lookupAtom var) branch of
                  Nothing     -> return Nothing
                  Just expr'  -> returnJust
                      ( RCaseAny
                      , st { code = expr' 
                           , astack = pushArgs [defvar] astack
                           }
                      )

      -- fixed
      rcasecon st@(StgState {..}) atoms expr = returnJust
          ( RCaseCon
          , st { code   = expr 
               , astack = pushArgs atoms astack
               }
          )

      -- fixed
      rprimop st op args = do
          returnJust 
              ( RPrimOP
              , st { code = applyPrimOp op (map lookupAtom args) }
              )
      
      -- fixed
      rpush st@(StgState {..}) ident args = returnJust 
          (RPush
          , st { code = EAtom (AVar ident)
               , cstack = map (CtArg . lookupAtom) args ++ cstack})


      -- fixed
      rret st@(StgState {..}) =
          let CtCase bs : rest = cstack
              EAtom a          = code
          in returnJust
            ( RRet
            , st { code = ECase (EAtom $ lookupAtom a) bs
                 , cstack = rest
                 , astack = popFrame astack})
                 
      -- intressant, dessutom fixad
      rthunk st@(StgState {..}) as size e v = returnJust      
          ( RThunk
          , st { code  = e
               , cstack = CtUpd v : cstack
               , astack = pushArgs as $ callFrame astack
               , heap   = H.insert v OBlackhole heap})
          
      -- fixad
      rupdate st@(StgState {..}) obj = case cstack of
            CtUpd  x : res -> returnJust 
                                 ( RUpdate
                                 , st { cstack = res
                                      , heap = H.insert x obj heap})
            CtOUpd x : res -> returnJust 
                                 ( RUpdate
                                 , st { cstack = res
                                      , heap = H.insertAbyss x obj heap})


      -- fixed
      rfenter st@(StgState {..}) args lenArgs size e = do
          let args' = map unArg $ take lenArgs cstack 
          -- args'' <- replicateM lenArgs newVar
          {- okey this is the thing...
             if we just subst names can clash => bad :/
             please if you can read this, save us callstack, you are our last hope
          -}
          returnJust
            ( RFEnter
            , st { code   = e  
                 , cstack = drop lenArgs cstack 
                 , astack = pushArgs args' (callFrame astack) })

      -- fixed
      rpap st@(StgState {..}) stackArgs var = do
          p <- newVar
          let args' = map (lookupAtom . unArg) $ take stackArgs cstack
              pap   = OPap var args'
          returnJust ( RPap1
                     , st { code   = EAtom (AVar (Heap p))
                          , cstack = drop stackArgs cstack
                          , heap   = H.insert p pap heap }
                     )
               
      -- fixed      
      rpenter st@(StgState {..}) var atoms = returnJust
          ( RPEnter
          , st { code   = EAtom (AVar (Heap var))
               , cstack = map CtArg atoms ++ cstack
               })
      
      
      roptimise st@(StgState {..}) omeg sets alpha = 
          returnJust
              ( ROptimise
              , st 
                  { code = EAtom omeg
                  , cstack = CtOpt alpha : cstack
                  , heap  = H.insert alpha OBlackhole heap
                  , settings = makeSettings astack heap sets : settings 
                  }
              )
      roptpap st@(StgState {..}) var atoms =
          case H.lookup var heap of
              Nothing -> error $ "OPTPAP: var not in heap: " -- ++ var
              Just (OFun args i e) ->
                  let (argsA, argsL) = splitAt (length atoms) args
                      CtOpt alpha : stack' = cstack
                      astack' = pushArgs (zipWith AUnknown [0..] argsL) (newFrame astack) 
                      --e' = subtractLocal var (length atoms) $ substList argsA (map lookupAtom atoms) e
                  in omega ROptPap (CtOFun argsL (i - length atoms) alpha:stack') astack' heap (inltM atoms 0 e) settings
              _ -> error "OPTPAP: pap doesn't point to FUN"
      roptfun st@(StgState {..}) args i expr = do
          let CtOpt alpha : cstack' = cstack
              astack'               = pushArgs (zipWith AUnknown [0..] args) (newFrame astack)
          omega (ROmega "from machine found fun to optimise") (CtOFun args i alpha : cstack') astack' heap expr settings
      
      

 {-     rcontopt st@(StgState {..}) = do
          let CtContOpt alpha : cstack' = cstack
          case H.lookup alpha heap of
              Nothing -> error $ "ContOpt rule: alpha not in heap buh huh: " ++ show alpha
              Just obj -> omega alpha cstack' heap obj 
-}
      rupdateopt st@(StgState {..}) obj var = do
          let CtOpt alpha : cstack' = cstack
          returnJust
              ( RUpdateOpt
              , st
                  { code  = EAtom (AVar var)
                  , cstack = cstack'
                  , heap  = H.insert alpha obj heap
                  }
              )
 
               
      rprintcon st@(StgState {..}) c atoms = do
          let CtPrint : cstack' = cstack
          case null atoms of
              True -> returnJust
                  ( RPrintCon
                  , st
                      { code = ESVal (SCon c [])
                      , cstack = cstack'})
              False -> returnJust
                  ( RPrintCon
                  , st
                      { code = EAtom (head atoms)
                      , cstack = CtPrint : CtPrintCon c [] (tail atoms) : cstack'}) 
      rprintval st@(StgState {..}) atom = do
          let CtPrint : cstack' = cstack
          returnJust
              ( RPrintVal
              , st 
                  { code  = ESVal (SAtom (lookupAtom atom))
                  , cstack = cstack'})
      rprintfun st@(StgState {..}) = do
          let CtPrint : cstack' = cstack
          returnJust
              ( RPrintFun
              , st
                  { code = ESVal SFun
                  , cstack = cstack'})
      rprintcont st@(StgState {..}) sval = do
          let CtPrintCon c ps ns : cstack' = cstack
          case ns of 
              [] -> returnJust
                  ( RPrintCont
                  , st
                      { code = ESVal (SCon c (reverse (sval:ps)))
                      , cstack = cstack'})
              n : ns -> returnJust
                  ( RPrintCont
                  , st 
                      { code = EAtom n
                      , cstack = CtPrint : CtPrintCon c (sval:ps) ns : cstack'})


{-
-- if we find anything that is point to a OThunk
-- we try to evalutate it
force st@(StgState {..}) = do 
  res <- step st 
  case res of
    Nothing -> case code of
        (EAtom var) | AVar (Heap v) <- lookupAtom var -> case H.lookup v heap of
            Nothing -> return $ show v
            Just (OThunk expr) -> force (st {code = expr}) 
            Just (OCon t []) -> return t 
            Just (OCon t atoms) 
                -> do list <- mapM (\x -> force (st {code = EAtom x})) atoms 
                      return $ "(" ++ t ++ " " ++ concat (space list) ++ ")"
        (EAtom (ANum n)) -> return $ show n
        other    -> return $ show other
    Just (r, st') -> force st'
  where
    space xs = let len = length xs - 1
      in map (++" ") (take len xs) ++ drop len xs

-- start the force evaluation
-- actually quite ugly
runForce :: Variable t => Input -> [Function t] -> t
runForce inp funs = evalStgM (go st) initialStgMState
  where
    gc = mkGC $ map mkcons $ [trueCon, falseCon]
    st = gc $ initialState (createGetFuns inp ++ funs)
    go st = do
        res <- step st
        case res of
            Nothing       -> force st
            Just (r, st') -> go st'
-}

eval :: Variable t => [Function t] -> [(Rule, StgState t)]
eval funs = (RInitial, st) : evalStgM (go st) initialStgMState
  where
    gc = mkGC $ map mkcons $ [trueCon, falseCon]
    st = gc $ initialState funs
    go st = do
        res <- step st
        case res of
            Nothing    -> return []
            Just (r, st') -> ((r, st') :) `fmap` go ({-gc-} st')
      

applyPrimOp :: Variable t => Primitive t -> [Atom t] -> Expr t
applyPrimOp op args = case (op,args) of 
    (IntOp {intOp  = (+)}, [ANum x,ANum y]) -> EAtom $ ANum $ x + y
    (IntCmp{intCmp = (+)}, [ANum x,ANum y]) -> EAtom $ avar $ x + y
    (DblOp {dblOp  = (+)}, [ADec x,ADec y]) -> EAtom $ ADec $ x + y
    (DblCmp{dblCmp = (+)}, [ADec x,ADec y]) -> EAtom $ avar $ x + y
    (ChrCmp{chrCmp = (+)}, [AChr x,AChr y]) -> EAtom $ avar $ x + y
    (MathFun{mathFun = f}, [ADec x])        -> EAtom $ ADec $ f x
    _                                       -> err
  where 
    avar = AVar . Heap . boolToCon
    err = 
        error $ "applyPrimOp: Primitive operation (" 
             ++ show op 
             ++ ") applied to arguments of the wrong type " 
             ++ "( " ++ show args ++ ")"
             ++ ", or not the correct number of arguments."
