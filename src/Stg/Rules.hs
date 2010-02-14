module STG.Rules
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
  | RPAP1
  | RPEnter
  

info :: Rule -> String
info RLet = "Let"
info _    = error "no info yet"