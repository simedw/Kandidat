module PrePrelude.PrePrelude
  ( prePrelude
  ) where

import Data.Char

import Stg.AST

prePrelude :: [Function String]
prePrelude =
    [ binOp "+"  PAdd
    , binOp "-"  PSub
    , binOp "*"  PMul
    , binOp "/"  PDiv
    , binOp "%"  PMod
    , binOp "<=" PLe
    , binOp "<"  PLt
    , binOp ">=" PGe
    , binOp ">"  PGt
    , binOp "==" PEq
    ]

-- Create the function for a primitive operation
binOp :: String -> Pop -> Function String
binOp name op = Function name $ OFun ["x", "y"] $
    ECase (EAtom (AVar "x")) 
        [BDef "x'" (ECase (EAtom (AVar "y")) 
            [BDef "y'" (EPop op [AVar "x'", AVar "y'"])])]

---- Create constructor functions (ex: true = True;)
--con :: String -> Function String
--jcon name = Function (map toLower name) 
         -- $ OCon name [] 
