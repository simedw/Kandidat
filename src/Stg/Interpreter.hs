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

--step :: (Data t, Ord t, Eq t, Show t) => StgState t -> StgM t (Maybe (Rule, StgState t))
step :: StgState String -> StgM String (Maybe (Rule, StgState String))
step st@(StgState code stack heap) = case code of
    ELet  b defs _ -> do
        vars <- replicateM (length defs) newVar
        let (ids, _objs) = unzip defs
            code'@(ELet _ defs' e') = substList ids (map AVar vars) code
            (_ids, objs) = unzip defs'
            heap' = foldr (\ (name, obj) h -> M.insert name obj h) 
                          heap
                          (zip vars objs)
        returnJust $ 
          (RLet, st { code = e'
          , heap = heap' })
    ECase e br     -> case e of
        EAtom (AVar v) ->
            case M.lookup v heap of
                Nothing -> return Nothing
                Just (OThunk _)     -> rcase
                Just (OCon c atoms) -> case instantiateBranch c atoms br of
                    Nothing -> rany
                    Just e  -> returnJust
                        ( RCaseCon
                        , st { code = e}
                        )
                _ -> rany
        EAtom (ANum n) -> rany
        _     -> rcase
      where
        rcase = returnJust $
          (RCaseCon
          , st { code  = e
               , stack = CtCase br : stack
               }
          )
        EAtom var = e
        rany = case findDefaultBranch var br of
            Nothing -> return Nothing
            Just e  -> returnJust
                ( RCaseAny
                , st { code = e }
                )
    EPop  op args  -> returnJust $ -- Primitive operation
        (RPrimOP
        , st { code = applyPrimOp op args }
        )
    ECall i args   -> returnJust $
        (RPush
        , st { code = EAtom (AVar i)
             , stack = map CtArg args ++ stack
             })
    EAtom (ANum _) | topUpd stack ->
        let CtUpd x : rest = stack
        in returnJust 
            ( RUpdate
            , st { stack = rest
            , heap  = M.insert x (OThunk code) heap}
            )
    EAtom (ANum _) | topCase stack -> 
        let CtCase bs : rest = stack
        in returnJust
          ( RRet
          , st { code = ECase code bs
               , stack = rest})
    EAtom (AVar v) -> case M.lookup v heap of  -- Is v on the heap
        Nothing  -> return Nothing
        Just obj -> case obj of
            OThunk e -> returnJust $           -- v is a thunk, apply rule Thunk
                ( RThunk
                , st { code  = e
                     , stack = CtUpd v : stack
                     , heap  = M.insert v OBlackhole heap})
            _ | topUpd stack -> let CtUpd x : rest = stack -- the object is a value, update memory
                                in  returnJust ( RUpdate
                                               , st { stack = rest
                                                    , heap  = M.insert x obj heap
                                                    }
                                               )
            _ | topCase stack -> let CtCase bs : rest = stack
                                 in returnJust
                                    ( RRet
                                    , st { code = ECase code bs
                                         , stack = rest
                                         }
                                    )
            OFun args e -> let lenArgs = length args   -- v is a fun
                               stackArgs = numArgs stack 
                in case lenArgs <= stackArgs of   
                    -- with enough arguments, apply FEnter
                    True -> let args' = map unArg $ take lenArgs stack -- Cannot handle arguments that are numbers
                             in returnJust
                                ( RFEnter
                                , st { code  = substList args args' e   -- since substList only replaces AVar to Avar
                                     , stack = drop lenArgs stack })
                    -- too few arguments, apply PAP1
                    False -> do
                        p <- newVar
                        let args' = map unArg $ take stackArgs stack
                            pap   = OPap v args' -- not here
                              in returnJust
                                 ( RPap1
                                 , st { code  = EAtom (AVar p)
                                      , stack = drop stackArgs stack
                                      , heap  = M.insert p pap heap })
            -- we have a new argument on the stack, apply PEnter
            OPap i atoms | topArg stack -> do
               returnJust
                 ( RPEnter
                 , st { code  = EAtom (AVar i)
                      , stack = map CtArg atoms ++ stack
                      , heap  = heap })
    _              -> return Nothing

eval :: [Function String] -> [(Rule, StgState String)]
eval funs = (RInitial, st) : evalState (go st) initialNames
  where
    gc = mkGC ["true", "false"]
    st = gc $ initialState funs
    go st = do
        res <- step st
        case res of
            Nothing    -> return []
            Just (r, st') -> ((r, st') :) `fmap` go (gc st')
      

applyPrimOp :: Pop -> [Atom String] -> Expr String
applyPrimOp op = case op of 
    PAdd -> num . binOp (+)
    PSub -> num . binOp (-)
    PMul -> num . binOp (*)
    PDiv -> num . binOp div
    PMod -> num . binOp mod
    PGe  -> con . binOp (>=)
    PGt  -> con . binOp (>)
    PLe  -> con . binOp (<=)
    PLt  -> con . binOp (<)
    PEq  -> con . binOp (==)
  where
    binOp op [ANum x, ANum y] = x `op` y
    con = flip ECall [] . map toLower . show 
    num = EAtom . ANum
