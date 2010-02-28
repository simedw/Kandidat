module Stg.Input where

import Control.Monad
import Stg.AST
import qualified Stg.PrePrelude as PP

data Input = Input
  { inputInteger  :: Maybe Integer
  , inputIntegers :: Maybe [Integer]
  }

defaultInput :: Input
defaultInput = Input { inputInteger = Nothing, inputIntegers = Nothing }

createGetFuns :: Input -> [Function String]
createGetFuns inp = 
    case inputInteger inp of
        Just n -> [Function "getInt" $ OCon PP.numCon [ANum n]]
        _ -> []
    ++
    case inputIntegers inp of
        Just [] ->
            [ Function "getIntList"
              $ OCon "Nil" []
            ]
        Just nums ->
            [ Function "getIntList"
                $ OThunk
                $ ELet False ((".n", OCon "Nil" []) : lets conVarNums ".n")
                    $ EAtom $ AVar $ fst $ last conVarNums ]
          where
            vars       = [1..] >>= flip replicateM ['a'..'z'] 
            varNums    = zip vars $ reverse nums
            conVars    = drop (length nums) vars
            conVarNums = zip conVars varNums
            lets [] _  = []
            lets ((conVar, (numVar, num)):rest) prev
                       = (numVar, OCon PP.numCon [ANum num])
                       : (conVar, OCon "Cons" [AVar numVar, AVar prev])
                       : lets rest conVar
        _ -> []
    
