{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PackageImports #-}
module Stg.Interpreter where

-- Naive Stg interpreter

import Data.Generics

import Data.Map(Map)
import qualified Data.Map as M
import "mtl" Control.Monad.State

import Text.PrettyPrint

import Parser.Pretty.Pretty

import Stg.AST
import Stg.Rules
import Stg.Substitution


data Cont t 
  = CtCase [Branch t]
  | CtUpd t
  | CtArg (Atom t)
 deriving Show

type Stack t = [Cont t]
type Heap  t = Map t (Obj t)

type StgM t = State [t]

-- | Create a fresh unbound variable
newVar :: StgM t t
newVar = do
    v <- get
    put (tail v)
    return (head v)

data StgState t = StgState
  { code  :: Expr  t
  , stack :: Stack t
  , heap  :: Map   t (Obj t)
  }
 -- deriving Show
 
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


instance Show t => Show (StgState t) where
  show st@(StgState {code, stack, heap}) = 
    "stack: " ++ show stack 
    ++ "\ncode: " ++ show (prExpr (text . show) code)
    ++ "\nheap: " ++ (concat [ show (id, prObj (text . show) obj) ++ "\n\t"
                             | (id, obj) <- M.toList heap])

initialState :: [Function String] -> StgState String
initialState funs = StgState
  { code  = getMain funs
  , stack = []
  , heap  = initialHeap funs
  }

initialNames :: [String]
initialNames = map ("i." ++) $ [1..] >>= flip replicateM ['a'..'z']


getMain = getFunction "main"

getFunction :: Eq t  => t -> [Function t] -> Expr t
getFunction s ((Function name o):xs) | s == name = let OThunk c = o in c
                                     | otherwise = getFunction s xs 
getFunction s [] = error "No main function"

initialHeap :: Ord t => [Function t] -> Heap t
initialHeap = M.fromList . map (\(Function name obj) -> (name, obj))

step :: (Data t, Ord t, Eq t, Show t) => StgState t -> StgM t (Maybe (Rule, StgState t))
step st@(StgState code stack heap) = case code of
    ELet  b defs _ -> do
        vars <- replicateM (length defs) newVar
        let (ids, _objs) = unzip defs
            code'@(ELet _ defs' e') = substList ids vars code
            (_ids, objs) = unzip defs'
            heap' = foldr (\ (name, obj) h -> M.insert name obj h) 
                          heap
                          (zip vars objs)
        returnJust $ 
          (RLet, st { code = e'
          , heap = heap' })
    ECase e br     -> undefined
    ECall i args   -> returnJust $
        (RPush
        , st { code = EAtom (AVar i)
             , stack = map CtArg args ++ stack
             })
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
                            pap   = OPap v (map AVar args')
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
    st = initialState funs
    go st = do
        res <- step st
        case res of
            Nothing    -> return []
            Just (r, st') -> ((r, st') :) `fmap` go st'
      