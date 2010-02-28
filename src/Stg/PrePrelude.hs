module Stg.PrePrelude
  ( prePrelude
  , numCon
  , decCon
  ) where

import Data.Char

import Stg.AST

numCon, decCon :: String
numCon = "I#"
decCon = "D#"

prePrelude :: [Function String]
prePrelude =
    [ binOp "+"  PAdd True
    , binOp "-"  PSub True
    , binOp "*"  PMul True
    , binOp "/"  PDiv True
    , binOp "%"  PMod True
    , binOp "<=" PLe  False
    , binOp "<"  PLt  False
    , binOp ">=" PGe  False
    , binOp ">"  PGt  False
    , binOp "==" PEq  False
    , alias "$"  "appl"
    , alias "."  "compose"
    , alias "++" "append"
    , alias ":"  "cons"
    , alias "&&" "and"
    , alias "||" "or"
    ]

-- Create a function alias
alias :: String -> String -> Function String
alias name = Function name . OThunk . EAtom . AVar

-- Create the function for a primitive operation
binOp :: String -> Pop -> Bool -> Function String
binOp name op box = Function name $ OFun ["x", "y"] $
    ECase (EAtom (AVar "x")) 
        [ branch numCon
        , branch decCon
        ]
  where
    branch c = BCon c ["x'"] 
             (ECase (EAtom (AVar "y"))
                 [ BCon c ["y'"] $ 
                     case box of
                         True  -> ECase (EPop op [AVar "x'", AVar "y'"] ) 
                             [ BDef "r" $ ELet False [("r'", OCon c [AVar "r"])] $ EAtom $ AVar "r'" ]
                         False -> EPop op [AVar "x'", AVar "y'"]
                 ]
             )
