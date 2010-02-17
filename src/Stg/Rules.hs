module Stg.Rules
 ( Rule(..)
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
 deriving (Show, Eq, Ord)

info :: Rule -> String
info RLet = "Let"
info _    = error "no info yet"
