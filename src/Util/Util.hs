module Util.Util where

dot :: (c -> d) -> (a -> b -> c) -> a -> b -> d
dot = (.) . (.)

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _         = error "fromRight: isLeft"