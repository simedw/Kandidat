{-# LANGUAGE FlexibleInstances #-}
module Parser.Pretty.Test where

import Stg.AST
import Parser.STGParser
import Parser.Pretty.Pretty

import Control.Applicative
import Control.Monad
import Test.QuickCheck

newtype ParsedSTGAST = PAST (Function String)
    deriving (Show)

prop_Pretty_Parse = forAllShrink arbitrary shrink $ \(PAST f) -> test_if_equal f

test_if_equal f = 
     case parseStg (show $ prFun f) of
        Right [f'] -> show f == show f'
        _ -> False

getId = flip suchThat (`notElem` keywords) 
    $ (:) <$> elements ['a' .. 'z'] <*> vars

getCon = flip suchThat (`notElem` keywords)
    $ (:) <$> elements ['A' .. 'Z'] <*> vars

vars = listOf (elements $ ['a' .. 'z'] ++ ['A' .. 'Z']) 

instance Arbitrary ParsedSTGAST where
    arbitrary = sized $ \s -> (\x y -> PAST $ Function x y) <$> getId <*> getObj s
    shrink (PAST (Function x o)) = map (PAST . Function x) $ shrinkObj o

instance Arbitrary (Obj String) where
    arbitrary = sized getObj
    shrink = shrinkObj

getObj 0 = oneof
    [ OCon <$> getId <*> listOf getAtom
    , pure OBlackhole
    ]
getObj s = oneof
    [ OFun <$> listOf1 getId <*> getExpr s'
    , OPap <$> getId <*> listOf1 getAtom
    , OCon <$> getCon <*> listOf getAtom
    , OOpt <$> getAtom
    , OThunk <$> getExpr s'
    , pure OBlackhole
    ]
    where s' = s `div` 2

getExpr 0 = oneof
    [ EAtom <$> getAtom
    , ECall <$> getId <*> listOf1 getAtom
    ]
getExpr s = oneof
    [ EAtom <$> getAtom
    , ECall <$> getId <*> listOf1 getAtom
    , ELet <$> arbitrary <*> getBinds s' <*> getExpr s'
    , ECase <$> getExpr s' <*> myListOf1 s getBranch
    ]
    where s' = s `div` 2

getAtom = oneof
    [ AVar <$> getId
    --, ANum <$> arbitrary
    ]

myListOf1 0 g = (:[]) <$> g 0
myListOf1 s g = do
    size <- choose (1, s)
    replicateM size (g $ s `div` size)

getBinds s = myListOf1 s $ \s' -> (,) <$> getId <*> getObj s'

getBranch s = oneof
    [ BCon <$> getId <*> listOf getId <*> getExpr s'
    , BDef <$> getId <*> getExpr s'
    ]
    where s' = s `div` 2

shrinkObj obj = case obj of
    OFun args e -> OBlackhole : [OThunk e' | e' <- shrinkExpr e]
        ++ [OFun args e' | e' <- shrinkExpr e]
    OPap id as -> []
    OCon _ _ -> []
    OThunk e -> [OThunk e' | e' <- shrinkExpr e]
    OOpt _ -> []
    OBlackhole -> []

shrinkExpr expr = case expr of
    EAtom _ -> []
    ECall _ _ -> []
    ELet b binds e -> e : [ELet b binds e' | e' <- shrinkExpr e]
        ++ [ELet b bs' e | bs' <- shrinkBinds binds]
        -- ++ [ELet b bs' e | bs' <- shrink binds]
    ECase scrut branches -> [scrut]

shrinkBinds bs = [ [ (t, o') | o' <- shrinkObj o]
                 | (t, o) <- bs
                 , not (null t)]

shrinkBranche br = case br of
    BCon t args e -> [BCon t args e' | e' <- shrinkExpr e]
    BDef t e -> [BDef t e' | e' <- shrinkExpr e]
