module Stg.Input where

import Control.Monad
import Stg.AST
import Stg.Variable

data Input = Input
  { inputInteger  :: Maybe Integer
  , inputIntegers :: Maybe [Integer]
  , inputDouble   :: Maybe Double
  , inputDoubles  :: Maybe [Double]
  , inputString   :: Maybe String
  }

instance Show Input where
    show (Input i is d ds s) = "("
      ++ showm i  " int: "
      ++ showm is " ints: "
      ++ showm d  " double: "
      ++ showm ds " doubles: "
      ++ showm s  " string:  "
      ++ ")"
      where showm (Just x) t = t ++ show x
            showm _        _ = ""

defaultInput :: Input
defaultInput = Input { inputInteger  = Nothing
                     , inputIntegers = Nothing
                     , inputDouble   = Nothing
                     , inputDoubles  = Nothing
                     , inputString   = Nothing
                     }


createGetFuns :: Variable t => Input -> [Function t]
createGetFuns inp = 
    case inputInteger inp of
        Just n -> [Function getInt $ OCon numCon [ANum n]]
        _ -> []
    ++
    case inputDouble inp of
        Just n -> [Function getDouble $ OCon decCon [ADec n]]
        _ -> []
    ++ 
    maybe [] ( \list -> [mkInputList list numCon getIntList ANum]) (inputIntegers inp)
    ++
    maybe [] ( \list -> [mkInputList list decCon getDoubleList ADec]) (inputDoubles inp)
    ++
    maybe [] ( \list -> [mkInputList list chrCon getString AChr]) (inputString inp)
   -- mkInputList [inputIntegers, inputDoubles, inputString] 
    {-
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
    -}

mkInputList nums con name f = case nums of
    []   -> Function name $ OCon nilCon []
    nums -> Function name
                $ OThunk [] 0
                 (mkELet False ((nil, OCon nilCon []) : lets conVarNums nil)
                    (EAtom $ AVar $ Heap $ fst $ last conVarNums))
  where
            (nil : vars)       = namesupply -- [1..] >>= flip replicateM ['a'..'z'] 
            varNums    = zip vars $ reverse nums
            conVars    = drop (length nums) vars
            conVarNums = zip conVars varNums
            lets [] _  = []
            lets ((conVar, (numVar, num)):rest) prev
                       = (numVar, OCon con [f num])
                       : (conVar, OCon consCon [AVar $ Heap numVar, AVar $ Heap prev])
                       : lets rest conVar



