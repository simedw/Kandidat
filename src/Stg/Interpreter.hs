{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PackageImports #-}
module Stg.Interpreter where

-- Naive Stg interpreter

import Data.Char
import Data.Generics
import Data.Maybe

import Data.Map(Map)
import qualified Data.Map as M
import "mtl" Control.Monad.State

import Stg.AST
import Stg.GC
import Stg.Input
import Stg.Rules
import Stg.Substitution
import Stg.Types


type StgM t = State [t]

-- | Create a fresh unbound variable
newVar :: StgM t t
newVar = do
    v <- get
    put (tail v)
    return (head v)
 
topCase :: Stack t -> Bool
topCase (CtCase _ : _) = True
topCase _              = False  

topArg :: Stack t -> Bool
topArg (CtArg _ : _) = True
topArg _             = False

topUpd :: Stack t -> Bool
topUpd (CtUpd _ : _) = True
topUpd _             = False

returnJust x = (return (Just (x)))

numArgs :: Stack t -> Int
numArgs = length . takeWhile isArg
  where 
    isArg (CtArg _) = True
    isArg _         = False
  
unArg :: Show t => Cont t -> Atom t
unArg (CtArg a) = a
unArg o = error $ "unArg: not an arg: " ++ show o 
instantiateBranch :: (Data t, Eq t) => t -> [Atom t] -> [Branch t] -> Maybe (Expr t)
instantiateBranch x atoms (BCon t ts e : bs) 
    | x == t    = Just $ substList ts atoms e
    | otherwise = instantiateBranch x atoms bs
instantiateBranch _ _ _ = Nothing

findDefaultBranch :: (Data t, Eq t) => Atom t -> [Branch t] -> Maybe (Expr t)
findDefaultBranch atom branches = listToMaybe [subst t atom e | BDef t e <- branches]


initialState :: [Function String] -> StgState String
initialState funs = gc $ StgState
  { code  = getMain funs
  , stack = []
  , heap  = initialHeap funs
  }
     where gc = id

initialNames :: [String]
initialNames = map ("i." ++) $ [1..] >>= flip replicateM ['a'..'z']


getMain = getFunction "main"

getFunction :: (Show t, Eq t)  => t -> [Function t] -> Expr t
getFunction s ((Function name o):xs) | s == name = let OThunk c = o in c
                                     | otherwise = getFunction s xs 
getFunction s [] = error $ "No \"" ++ show s  ++ "\" function"

initialHeap :: Ord t => [Function t] -> Heap t
initialHeap = M.fromList . map (\(Function name obj) -> (name, obj))



step :: StgState String -> StgM String (Maybe (Rule, StgState String))
step st@(StgState code stack heap) = case code of
    ELet recursive defs _  -> rlet st recursive defs 
    ECase expr branch      -> case expr of
        EAtom (AVar var)  ->
            case M.lookup var heap of
                Nothing             -> return Nothing
                Just (OThunk _)     -> rcase expr branch
                Just (OCon c atoms) -> case instantiateBranch c atoms branch of
                    Nothing   -> rany expr branch
                    Just expr -> rcasecon st expr 
                _                   -> rany expr branch
        EAtom a          -> rany  expr branch
        _                -> rcase expr branch

    EPop  op args     -> rprimop st op args 
    ECall ident args  -> rpush st ident args 
    EAtom a | not (isVar a) && topUpd stack  -> rupdate st (OThunk code)
            | not (isVar a) && topCase stack -> rret st
    EAtom (AVar var) -> case M.lookup var heap of  
        Nothing  -> return Nothing
        Just obj -> case obj of
            OThunk expr       -> rthunk st expr var 
            _ | topUpd  stack -> rupdate st obj
            _ | topCase stack -> rret st 
            OFun args expr    -> let lenArgs = length args
                                     stackArgs = numArgs stack 
                -- depending on the number of arguments the function is either
                -- saturated or not 
                in case lenArgs <= stackArgs of
                    True  -> rfenter st args lenArgs expr
                    False -> rpap st stackArgs var 
            OPap ident atoms | topArg stack -> rpenter st ident atoms     
            _                               -> return Nothing
    -- if there is no rule to apply, do nothing
    _              -> return Nothing
  where
    rlet st@(StgState code stack heap) recursive defs = do
        vars <- replicateM (length defs) newVar
        let (ids, _objs) = unzip defs
            code'@(ELet _ defs' e') = substList ids (map AVar vars) code
            (_ids, objs) = unzip defs'
            heap' = foldr (\ (name, obj) h -> M.insert name obj h) 
                          heap
                          (zip vars objs)
        returnJust $ (RLet, st { code = e'
                          , heap = heap' })
                          
    rcase expr branch = returnJust $
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

    rprimop st op args = returnJust $
        ( RPrimOP
        , st { code = applyPrimOp op args }
        )
    
    rpush st@(StgState code stack heap) ident args = returnJust $
        (RPush
        , st { code = EAtom (AVar ident)
             , stack = map CtArg args ++ stack})

    rret st@(StgState code stack heap) =
        let CtCase bs : rest = stack
        in returnJust
          ( RRet
          , st { code = ECase code bs
               , stack = rest})
    rthunk st@(StgState code stack heap) e v = returnJust $     
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
        returnJust $ 
                      ( RPap1
                      , st { code  = EAtom (AVar p)
                      , stack = drop stackArgs stack
                      , heap  = M.insert p pap heap })
    rpenter st@(StgState code stack heap) var atoms = returnJust
        ( RPEnter
        , st { code  = EAtom (AVar var)
        , stack = map CtArg atoms ++ stack
        , heap  = heap })
 

eval :: Input -> [Function String] -> [(Rule, StgState String)]
eval inp funs = (RInitial, st) : evalState (go st) initialNames
  where
    gc = mkGC ["$True", "$False"]
    st = gc $ initialState (createGetFuns inp ++ funs)
    go st = do
        res <- step st
        case res of
            Nothing    -> return []
            Just (r, st') -> ((r, st') :) `fmap` go (gc st')
      

applyPrimOp :: Pop -> [Atom String] -> Expr String
applyPrimOp op = case op of 
    PAdd -> binOp (+) (+)
    PSub -> binOp (-) (-)
    PMul -> binOp (*) (*)
    PDiv -> binOp div (/)
    PMod -> binOp mod modError --(%.)
    PGe  -> binOpCon (>=) (>=)
    PGt  -> binOpCon (>)  (>)
    PLe  -> binOpCon (<=) (<=)
    PLt  -> binOpCon (<)  (<)
    PEq  -> binOpCon (==) (==)
  where
    applyBinOp nf _  nop _   [ANum x, ANum y] = EAtom . nf $ x `nop` y
    applyBinOp _  df _   dop [ADec x, ADec y] = EAtom . df $ x `dop` y
    applyBinOp _  _  _   _   args = 
        error $ "applyPrimOp: Primitive operation (" 
             ++ show op 
             ++ ") applied to arguments of the wrong type (" 
             ++ show args ++ "), or not the correct number of arguments."

    binOpCon = applyBinOp mkConFun mkConFun
    binOp nop dop = applyBinOp ANum ADec nop dop
    modError = error "applyPrimOp: mod (%) operation not supported on Doubles."
    mkConFun = AVar . ('$':) . show
    --x %. y = fromIntegral $ truncate x `mod` truncate y
