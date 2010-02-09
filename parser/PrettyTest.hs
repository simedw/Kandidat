module PrettyTest where

import AST
import STGParser
import Pretty

import Control.Applicative
import Control.Monad
import Test.QuickCheck

newtype ParsedSTGAST = PAST (Function String)
    deriving (Show)

prop_Pretty_Parse (PAST f) = case parseStg (show $ prFun f) of
    Right [f'] -> show f == show f'
    _ -> False

getId = listOf1 (elements $ ['a' .. 'z'] ++ ['A' .. 'Z'])

instance Arbitrary ParsedSTGAST where
    arbitrary = sized $ \s -> (\x y -> PAST $ Function x y) <$> getId <*> getObj s

getObj 0 = oneof
    [ OCon <$> getId <*> listOf getAtom
    , pure OBlackhole
    ]
getObj s = oneof
    [ OFun <$> listOf1 getId <*> getExpr s'
    --, OPap <$> getObj <*> listOf1 getAtom
    , OCon <$> getId <*> listOf getAtom
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
    , ECase <$> getExpr s' <*> listOf1 (getBranch s')
    ]
    where s' = s `div` 2

getAtom = oneof
    [ AVar <$> getId
    --, ANum <$> arbitrary
    ]

getBinds s = listOf1 $ (,) <$> getId <*> getObj s'
    where s' = s `div` 2

getBranch s = oneof
    [ BCon <$> getId <*> listOf getId <*> getExpr s'
    , BDef <$> getId <*> getExpr s'
    ]
    where s' = s `div` 2
