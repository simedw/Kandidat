module Stg.Input where

import Control.Monad
import Stg.AST
import Stg.Variable
import qualified Stg.PrePrelude as PP

data Input = Input
  { inputInteger  :: Maybe Integer
  , inputIntegers :: Maybe [Integer]
  }

defaultInput :: Input
defaultInput = Input { inputInteger = Nothing, inputIntegers = Nothing }

createGetFuns :: Variable t => Input -> [Function t]
createGetFuns inp = 
    case inputInteger inp of
        Just n -> [Function getInt $ OCon numCon [ANum n]]
        _ -> []
    ++
    case inputIntegers inp of
        Just [] ->
            [ Function getIntList
              $ OCon nilCon []
            ]
        Just nums ->
            [ Function getIntList
                $ OThunk [] 0
                 (mkELet False ((nil, OCon nilCon []) : lets conVarNums nil)
                    (EAtom $ AVar $ Heap $ fst $ last conVarNums)) ]
          where
            (nil : vars)       = namesupply -- [1..] >>= flip replicateM ['a'..'z'] 
            varNums    = zip vars $ reverse nums
            conVars    = drop (length nums) vars
            conVarNums = zip conVars varNums
            lets [] _  = []
            lets ((conVar, (numVar, num)):rest) prev
                       = (numVar, OCon numCon [ANum num])
                       : (conVar, OCon consCon [AVar $ Heap numVar, AVar $ Heap prev])
                       : lets rest conVar
        _ -> []
    
