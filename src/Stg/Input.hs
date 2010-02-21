module Stg.Input where

import Control.Monad
import Stg.AST

data Input = Input
  { inputInteger  :: Maybe Integer
  , inputIntegers :: Maybe [Integer]
  }

defaultInput :: Input
defaultInput = Input { inputInteger = Nothing, inputIntegers = Nothing }

createGetFuns :: Input -> [Function String]
createGetFuns inp = 
    case inputInteger inp of
        Just n -> [Function "getInt" $ OThunk (EAtom (ANum n))]
        _ -> []
    ++
    case inputIntegers inp of
        Just [] ->
            [ Function "getIntList"
              $ OCon "Nil" []
            ]
        Just ns -> 
            [ Function "getIntList" 
                $ OThunk 
                $ ELet False ((".n", OCon "Nil" []) : lets as ".n") 
                    $ EAtom $ AVar (fst (last as))]
          where
            as = zip ([1..] >>= flip replicateM ['a'..'z']) (reverse ns)
            lets ((v, n):ns) prev = (v, OCon "Cons" [ANum n, AVar prev]) : lets ns v
            lets [] _ = []
        _ -> []
    
