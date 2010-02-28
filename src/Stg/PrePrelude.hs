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
    [ binbox  "+" (+) (+)
    , binbox  "-" (-) (-)
    , binbox  "*" (*) (*)
    , binbox  "/" div (/)
    , binbox  "%" mod (notSupportedError "%")
    , binbool "<=" (<=) (<=)
    , binbool ">=" (>=) (>=)
    , binbool "<"  (<)  (<)
    , binbool ">"  (>)  (>)
    , binbool "==" (==) (==)
    , un "sin"  (notSupportedError "sin")  sin
    , un "cos"  (notSupportedError "cos")  cos
    , un "tan"  (notSupportedError "tan")  tan
    , un "atan" (notSupportedError "atan") atan
    , un "sqrt" (notSupportedError "sqrt") sqrt
    , un "log"  (notSupportedError "log")  log
    , un "exp"  (notSupportedError "exp")  exp
    ]
  where
    binbox  name = binOp True  name `dot` PBinOp   name 
    binbool name = binOp False name `dot` PBinBool name
    un      name = unOp  True  name `dot` PUnOp    name
    notSupportedError op args = 
        error $ "Operation " 
             ++ show op 
             ++ " not supported on arguments of the given type " 
             ++ "(" ++ show args ++ ")"
    dot = (.) . (.)

-- Create a function alias
alias :: String -> String -> Function String
alias name = Function name . OThunk . EAtom . AVar

-- Create the function for a primitive unary operation
unOp :: Bool -> String -> Pop String -> Function String
unOp box name op = Function name $ OFun ["x"] $
    ECase (EAtom (AVar "x"))
        [ branch numCon
        , branch decCon
        ]
  where
    branch c = BCon c ["x'"] $
             case box of
                 True -> ECase (EPop op [AVar "x'"])
                     [ BDef "r" 
                         $ ELet False [("r'", OCon c [AVar "r"])] 
                         $ EAtom $ AVar "r'" 
                     ]
                 False -> EPop op [AVar "x'"]

-- Create the function for a primitive binary operation
binOp :: Bool -> String -> Pop String -> Function String
binOp box name op = Function name $ OFun ["x", "y"] $
    ECase (EAtom (AVar "x")) 
        [ branch numCon
        , branch decCon
        ]
  where
    branch c = BCon c ["x'"] 
             (ECase (EAtom (AVar "y"))
                 [ BCon c ["y'"] $ 
                     case box of
                         True  -> ECase (EPop op [AVar "x'", AVar "y'"]) 
                             [ BDef "r" 
                                 $ ELet False [("r'", OCon c [AVar "r"])] 
                                 $ EAtom $ AVar "r'" 
                             ]
                         False -> EPop op [AVar "x'", AVar "y'"]
                 ]
             )
