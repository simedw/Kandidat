{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}
module Stg.Interpreter where

-- Naive Stg interpreter

import Control.Monad
import "mtl" Control.Monad.State

import Data.Char
import Data.Generics
import Data.Maybe

import Data.Map(Map)
import qualified Data.Map as M

import Stg.AST
import Stg.GC
import Stg.Input
import Stg.Optimise
import Stg.Rules
import Stg.Substitution
import Stg.Types
import Stg.Branch
 
topCase :: Stack t -> Bool
topCase (CtCase _ : _) = True
topCase _              = False  

topArg :: Stack t -> Bool
topArg (CtArg _ : _) = True
topArg _             = False

topUpd :: Stack t -> Bool
topUpd (CtUpd _ : _) = True
topUpd _             = False

topOpt :: Stack t -> Bool
topOpt (CtOpt _ : _) = True
topOpt _             = False

{-
topContOpt :: Stack t -> Bool
topContOpt (CtContOpt _ : _) = True
topContOpt _                 = False
-}

topPrint :: Stack t -> Bool
topPrint (CtPrint : _) = True
topPrint _             = False

topPrintCon :: Stack t -> Bool
topPrintCon (CtPrintCon _ _ _ : _) = True
topPrintCon _                      = False

topIsO :: Stack t -> Bool
topIsO (cont : _) = case cont of
    CtOFun  _ _     -> True
    CtOCase _       -> True
    CtOLetObj _ _   -> True
    CtOLetThunk _ _ -> True
    _               -> False
topIsO _          = False

topOInstant :: Stack t -> Bool
topOInstant (CtOInstant n: _) = n <= 0
topOInstant _                = False

decTop :: Stack t -> Stack t
decTop (CtOInstant n : xs) = CtOInstant (n-1) : xs
decTop xs = xs 

numArgs :: Stack t -> Int
numArgs = length . takeWhile isArg
  where 
    isArg (CtArg _) = True
    isArg _         = False
  
unArg :: Show t => Cont t -> Atom t
unArg (CtArg a) = a
unArg o = error $ "unArg: not an arg: " ++ show o 


initialState :: [Function String] -> StgState String
initialState funs = gc StgState
  { code  = getMain funs
  , stack = []
  , heap  = initialHeap funs
  }
     where gc = id

initialNames :: [String]
initialNames = map ("i." ++) $ [1..] >>= flip replicateM ['a'..'z']

initialStgMState :: StgMState String
initialStgMState = StgMState
    { nameSupply = initialNames
    , mkCons     = ('$' :)
    }

getMain = getFunction "main"

getFunction :: (Show t, Eq t)  => t -> [Function t] -> Expr t
getFunction s ((Function name o):xs) | s == name = let OThunk c = o in c
                                     | otherwise = getFunction s xs 
getFunction s [] = error $ "No \"" ++ show s  ++ "\" function"

initialHeap :: Ord t => [Function t] -> Heap t
initialHeap = M.fromList . map (\(Function name obj) -> (name, obj))


step :: (Ord t, Data t, Show t) => StgState t -> StgM t (Maybe (Rule, StgState t))
step st'@(StgState code (decTop -> stack) heap) = case code of
    _ | topOInstant stack -> omega (drop 1 stack) heap code -- for inlining
    ECase expr branch     -> case expr of
        EAtom (AVar var)  ->
            case M.lookup var heap of
                Nothing             -> error "var not in heap" -- return Nothing
                Just (OThunk _)     -> rcase expr branch
                Just (OCon c atoms) -> case instantiateBranch c atoms branch of
                    Nothing   -> rany expr branch
                    Just expr -> rcasecon st expr 
                _                   -> rany expr branch
        EAtom a          -> rany  expr branch
        _                -> rcase expr branch
    ELet recursive defs _  -> rlet st recursive defs 
    EPop  op args     -> rprimop st op args 
    ECall ident args  -> rpush st ident args 
    EAtom a | not (isVar a) && topUpd stack  -> rupdate st (OThunk code)
            | not (isVar a) && topCase stack -> rret st
            | not (isVar a) && topPrint stack -> rprintval st a
    EAtom (AVar var) -> case M.lookup var heap of  
        Nothing  -> return Nothing
        Just obj -> case obj of
            OThunk expr       -> rthunk st expr var
            _ | topIsO stack  -> psi stack heap var
            OOpt   alpha      -> roptimise st alpha var
            _ | topUpd  stack -> rupdate st obj
            _ | topCase stack -> rret st
--            _ | topContOpt stack -> rcontopt st
            OCon t atoms | topPrint stack -> rprintcon st t atoms
            _            | topPrintCon stack -> rprintfun st
            OPap ident atoms | topArg stack -> rpenter st ident atoms
                             | topOpt stack -> roptpap st ident atoms
            _ | topOpt stack  -> rupdateopt st obj var
            OFun args expr    -> let lenArgs = length args
                                     stackArgs = numArgs stack 
                -- depending on the number of arguments the function is either
                -- saturated or not 
                in case lenArgs <= stackArgs of
                    True  -> rfenter st args lenArgs expr
                    False -> rpap st stackArgs var 
            _                               -> return Nothing
    ESVal sval | topPrintCon stack -> rprintcont st sval
    -- if there is no rule to apply, do nothing
    _              -> return Nothing
  where
    st = st' { stack = stack}
    rlet st@(StgState code stack heap) recursive defs = do
        vars <- replicateM (length defs) newVar
        let (ids, _objs) = unzip defs
            code'@(ELet _ defs' e') = substList ids (map AVar vars) code
            (_ids, objs) = unzip defs'
            heap' = foldr (uncurry M.insert) heap (zip vars objs)
        returnJust (RLet, st { code = e'
                             , heap = heap' })
                          
    rcase expr branch = returnJust 
          (RCaseCon
          , st { code  = expr
               , stack = CtCase branch : stack
               }
          )
    rany (EAtom var) branch = case findDefaultBranch var branch of
                Nothing     -> return Nothing
                Just expr'  -> returnJust
                    ( RCaseAny
                    , st { code = expr' }
                    )
    rcasecon st@(StgState code stack heap) expr = returnJust
        ( RCaseCon
        , st { code = expr}
        )

    rprimop st op args = do
        mkConFun <- gets mkCons
        returnJust 
            ( RPrimOP
            , st { code = applyPrimOp mkConFun op args }
            )
    
    rpush st@(StgState code stack heap) ident args = returnJust 
        (RPush
        , st { code = EAtom (AVar ident)
             , stack = map CtArg args ++ stack})

    rret st@(StgState code stack heap) =
        let CtCase bs : rest = stack
        in returnJust
          ( RRet
          , st { code = ECase code bs
               , stack = rest})
    rthunk st@(StgState code stack heap) e v = returnJust      
        ( RThunk
        , st { code  = e
        , stack = CtUpd v : stack
        , heap  = M.insert v OBlackhole heap})
    rupdate st@(StgState code stack heap) obj = 
        let CtUpd x : rest = stack -- the object is a value, update memory
        in  returnJust ( RUpdate
                       , st { stack = rest
                       , heap  = M.insert x obj heap})
    rfenter st@(StgState code stack heap) args lenArgs e = 
        let args' = map unArg $ take lenArgs stack 
        in returnJust
                       ( RFEnter
                       , st { code  = substList args args' e  
                            , stack = drop lenArgs stack })

    rpap st@(StgState code stack heap) stackArgs var = do
        p <- newVar
        let args' = map unArg $ take stackArgs stack
            pap   = OPap var args'
        returnJust ( RPap1
                   , st { code  = EAtom (AVar p)
                        , stack = drop stackArgs stack
                        , heap  = M.insert p pap heap }
                   )
    rpenter st@(StgState code stack heap) var atoms = returnJust
        ( RPEnter
        , st { code  = EAtom (AVar var)
        , stack = map CtArg atoms ++ stack
        , heap  = heap })
    roptimise st@(StgState code stack heap) omeg alpha = returnJust
        ( ROptimise
        , st 
            { code = EAtom omeg
            , stack = CtOpt alpha : stack
            , heap  = M.insert alpha OBlackhole heap
            }
        )
    roptpap st@(StgState code stack heap) var atoms =
        case M.lookup var heap of
            Nothing -> error $ "OPTPAP: var not in heap: " -- ++ var
            Just (OFun args e) ->
                let (argsA, argsL) = splitAt (length atoms) args
                    CtOpt alpha : stack' = stack
                    e' = substList argsA atoms e
                in omega (CtOFun argsL alpha:stack') heap e'
            _ -> error "OPTPAP: pap doesn't point to FUN"
{-
    rcontopt st@(StgState code stack heap) = do
        let CtContOpt alpha : stack' = stack
        case M.lookup alpha heap of
            Nothing -> error $ "ContOpt rule: alpha not in heap buh huh: " ++ show alpha
            Just obj -> omega alpha stack' heap obj 
-}
    rupdateopt st@(StgState code stack heap) obj var = do
        let CtOpt alpha : stack' = stack
        returnJust
            ( RUpdateOpt
            , st
                { code  = EAtom (AVar var)
                , stack = stack'
                , heap  = M.insert alpha obj heap
                }
            ) 
    rprintcon st@(StgState code stack heap) c atoms = do
        let CtPrint : stack' = stack
        case null atoms of
            True -> returnJust
                ( RPrintCon
                , st
                    { code = ESVal (SCon c [])
                    , stack = stack'})
            False -> returnJust
                ( RPrintCon
                , st
                    { code = EAtom (head atoms)
                    , stack = CtPrint : CtPrintCon c [] (tail atoms) : stack'}) 
    rprintval st@(StgState code stack heap) atom = do
        let CtPrint : stack' = stack
        returnJust
            ( RPrintVal
            , st 
                { code  = ESVal (SAtom atom)
                , stack = stack'})
    rprintfun st@(StgState code stack heap) = do
        let CtPrint : stack' = stack
        returnJust
            ( RPrintFun
            , st
                { code = ESVal SFun
                , stack = stack'})
    rprintcont st@(StgState code stack heap) sval = do
        let CtPrintCon c ps ns : stack' = stack
        case ns of 
            [] -> returnJust
                ( RPrintCont
                , st
                    { code = ESVal (SCon c (reverse (sval:ps)))
                    , stack = stack'})
            n : ns -> returnJust
                ( RPrintCont
                , st 
                    { code = EAtom n
                    , stack = CtPrint : CtPrintCon c (sval:ps) ns : stack'})



-- if we find anything that is point to a OThunk
-- we try to evalutate it
force st@(StgState code stack heap) = do 
  res <- step st 
  case res of
    Nothing -> case code of
        (EAtom (AVar v)) -> case M.lookup v heap of
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
runForce :: Input -> [Function String] -> String
runForce inp funs = evalState (go st) initialStgMState
  where
    gc = mkGC ["$True", "$False"]
    st = gc $ initialState (createGetFuns inp ++ funs)
    go st = do
        res <- step st
        case res of
            Nothing       -> force st
            Just (r, st') -> go st'
 

eval :: Input -> [Function String] -> [(Rule, StgState String)]
eval inp funs = (RInitial, st) : evalState (go st) initialStgMState
  where
    gc = mkGC ["$True", "$False"]
    st = gc $ initialState (createGetFuns inp ++ funs)
    go st = do
        res <- step st
        case res of
            Nothing    -> return []
            Just (r, st') -> ((r, st') :) `fmap` go (gc st')
      

applyPrimOp :: Show t => (String -> t) -> Pop t -> [Atom t] -> Expr t
applyPrimOp mkCons op = case op of 
    PBinOp   _ nop dop -> applyBinOp ANum ADec nop dop
    PUnOp    _ nop dop -> applyUnOp  ANum ADec nop dop
    PBinBool _ nop dop -> applyBinOp mkConFun mkConFun nop dop
  where
    applyBinOp nf _  nop _   [ANum x, ANum y] = EAtom . nf $ x `nop` y
    applyBinOp _  df _   dop [ADec x, ADec y] = EAtom . df $ x `dop` y
    applyBinOp _ _ _ _ args = err args
    applyUnOp nf _  nop _   [ANum x] = EAtom . nf $ nop x
    applyUnOp _  df _   dop [ADec x] = EAtom . df $ dop x
    mkConFun = AVar . mkCons . show
    err args = 
        error $ "applyPrimOp: Primitive operation (" 
             ++ show op 
             ++ ") applied to arguments of the wrong type " 
             ++ "( " ++ show args ++ ")"
             ++ ", or not the correct number of arguments."
