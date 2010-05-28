module Shared.BoxPrimitives where

import Shared.Primitives
import Stg.AST
import Stg.Variable

prePrelude :: [Function String]
prePrelude = map boxPrimFun $ primitives ++ mathFunctions

relatedBox :: Variable t => Primitive t -> t  
relatedBox (IntOp{})   = numCon
relatedBox (IntCmp{})  = numCon
relatedBox (DblOp{})   = decCon
relatedBox (DblCmp{})  = decCon
relatedBox (ChrCmp{})  = chrCon
relatedBox (MathFun{}) = decCon

boxPrimFun :: Variable t => Primitive t -> Function t
boxPrimFun prim 
    | isUnary prim = Function (opDesc prim) $ OFun [x] 1 $
        ECase (EAtom $ aHeap x) 
            [ BCon c [x'] $ ECase (EPop prim [aHeap x'])
                [ BDef r $ ELet (NonRec r' $ OCon c [aHeap r])
                         $ EAtom $ aHeap r'  
                ] 
            ]
    | otherwise = Function (opDesc prim) $ OFun [x,y] 2 $
        ECase (EAtom $ aHeap x) 
            [ BCon c [x'] $ ECase (EAtom $ aHeap y) 
                [ BCon c [y'] $ ECase (EPop prim [aHeap x', aHeap y'])
                    [ BDef r $ 
                        if isCmp prim 
                            then EAtom $ aHeap r
                            else ELet (NonRec r' $ OCon c [aHeap r])
                               $ EAtom $ aHeap r'  
                    ] 
                ]
            ]
  where 
    x:y:x':y':r:r':_ = namesupply
    c = relatedBox prim
    aHeap = AVar . Heap
