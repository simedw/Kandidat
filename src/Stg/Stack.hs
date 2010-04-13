module Stg.Stack where

import Stg.AST
import Stg.Variable
type ArgStack t = [StackFrame t] 

data StackFrame t = StackFrame 
    { lock :: Int
    , spointer :: Int -- Wohoo this is hmm
    , args :: [Atom t]    -- should not be exported
    } deriving Show 

lookupStackFrame :: Int -> ArgStack t -> Atom t
lookupStackFrame x (a:_) = args a !! x

newStackFrame :: Variable t => Int -> ArgStack t -> ArgStack t
newStackFrame size as = StackFrame 0 0 (replicate size (error "nullpointer : ⊥")) : remove as
  where
    remove (StackFrame 0 _ _ : as) = as
    remove s                       = s 
pushArg :: Atom t -> Int -> ArgStack t -> ArgStack t
pushArg x p (a:as) = let atoms = args a
                     in  a { args = take p atoms ++ x : drop (p+1) atoms } : as

pushArgs :: [Atom t] -> ArgStack t -> ArgStack t
pushArgs xs s = (foldr (\a as -> incrSPointer (pushArg a (getSPointer as) as))) s (reverse xs)

-- | Increase the lock on the stack frame, i.e we will need it later                     
incrLock :: ArgStack t -> ArgStack t
incrLock (a:as) = a { lock = lock a + 1 } : as

-- | Removes the stack frame if lock becomes zero (is popStackFrame needed?)
decrLock :: ArgStack t -> ArgStack t
decrLock (a:as) = 
    let lock' = lock a - 1
    in a { lock = max 0 lock' } : as

getSPointer :: ArgStack t -> Int
getSPointer (StackFrame _ sp _ : _) = sp

incrSPointer :: ArgStack t -> ArgStack t
incrSPointer (StackFrame lock sp as :as') = StackFrame lock (sp + 1) as : as'
