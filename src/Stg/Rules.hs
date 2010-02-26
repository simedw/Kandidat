module Stg.Rules
 ( Rule(..)
 , OptRule(..)
 , info 
 ) where

data Rule
  = RLet
  | RCaseCon
  | RCaseAny
  | RCase
  | RRet
  | RThunk
  | RUpdate
  | RKnownCall
  | RPrimOP
  | RPush
  | RFEnter
  | RPap1
  | RPEnter
  | RInitial
  | ROptimise
  | ROptPap
  | RUpdateOpt
  | ROpt OptRule
  | RContOpt
 deriving (Show, Eq, Ord, Read)

data OptRule
  = ORKnownCall
  | ORKnownCase
  | ORDone
 deriving (Show, Eq, Ord, Read)

info :: Rule -> String
info RLet = "Let"
info _    = error "no info yet"
