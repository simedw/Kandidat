module PrePrelude.PrePrelude
  ( prePrelude
  ) where

import Stg.AST

prePrelude :: [Function String]
prePrelude =
    [ binOp "+" PPlus
    , binOp "-" PMinus
    ]

-- Create the function for a primitive operation
binOp :: String -> Pop -> Function String
binOp name op = Function name $ OFun ["x", "y"] $
    ECase (EAtom (AVar "x")) 
        [BDef "x'" (ECase (EAtom (AVar "y")) 
            [BDef "y'" (EPop op [AVar "x'", AVar "y'"])])]
