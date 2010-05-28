module Regexp where

data Regexp a
    = Empty 
    | Symbol a
    | Seq (Regexp a) (Regexp a)
    | Union (Regexp a) (Regexp a)
    | Star (Regexp a)
 deriving Show

-- (ab)* = Star (Seq (Symbol a) (Symbol a))

data List a
   = Cons a (List a)
   | Nil
 deriving (Show, Eq)


toList = foldr Cons Nil

data Res a
  = Failed
  | Success (List a)
 deriving Show


ex = Star (Symbol 'a')
ex2 = Star (Symbol 'a' `Union` Symbol 'b')  -- (a|b)*
ex3 = Star (Symbol 'a' `Seq` Symbol 'b')  -- (ab)*
ex4 = Symbol 'a' `Union` Star (Symbol 'b')
ex5 = Symbol 'a' `Seq` (Star (Symbol 'b')) `Seq` (Symbol 'c' `Union` Empty) -- (ab*(c|e) 

empty Nil = True
empty _   = False
{-
mothermatch p l = case match p l of
    { Success xs  -> empty xs
    ; _           -> False
    }
-}

concatMap' :: (a -> List b) -> List a -> List b 
concatMap' f Nil = Nil
concatMap' f (Cons x xs) = (f x) +++ (concatMap' f xs)

concat' :: (List (List a)) -> List a
concat' Nil = Nil
concat' (Cons x xs) = x +++ concat' xs

(+++) :: List a -> List a -> List a
Nil +++ xs = xs
(Cons x xs) +++ ys = Cons x (xs +++ ys)

map' f Nil = Nil
map' f (Cons x xs) = Cons (f x) (map' f xs)

s :: List a -> List (List a)
s x = Cons x Nil
{-
match :: Eq a => Regexp a -> List a -> List (List a)
match r l = case r of
    { Empty -> s l
    ; Symbol a -> case l of
        Cons x xs -> case (x==a) of
                      { True  -> s xs
                      ; False -> Nil
                      }
        Nil       -> Nil
    ; Star p -> case match p l of
                { Nil -> s l   -- no match
                ; xs -> concatMap' (match (Star p)) xs
                }
    ; Union p q -> concat' $ s (match p l) +++ s (match q l)
    ; Seq p q -> case match p l of
            { Nil -> Nil
            ; xs -> concatMap' (match q) xs
            }
    }
-}
-- parse :: String -> [Regexp Char] -> Regexp Char
parse cs = case parse' cs [] of
    [x] -> x
    _   -> error "too long"

parse' cs stack = case cs of
    { Cons ',' rest  -> parse' rest (Empty : stack)
    ; Cons '*' rest  -> parse' rest (Star (head stack) : tail stack)
    ; Cons '.' rest  -> parse' rest (Seq (head $ tail stack) (head stack) : drop 2 stack)
    ; Cons '|' rest  -> parse' rest (Union (head $ tail stack) (head stack) : drop 2 stack)
    ; Cons c   rest  -> parse' rest (Symbol c : stack)
    ; Nil       -> stack 
    }

c x = Cons x Nil

match :: Eq a => Regexp a -> List a -> List (Either (List a) (List a))
match r l = case r of
    { Empty -> c (Right l)
    ; Symbol a -> case l of
        Cons x xs -> case (x==a) of
                      { True  -> c (Right xs)
                      ; False -> c (Left l)
                      }
        _       -> c (Left l)
    ; Star p -> case match p l of
                {
                 xs -> concatMap' (either (\x -> c (Right x)) (match (Star p))) xs
                }
    ; Union p q ->  (match p l) +++ (match q l)
    ; Seq p q -> case match p l of
            {  xs -> concatMap' (either (\x -> c (Left x)) (match q)) xs
            }
    }

elem' y Nil = False
elem' y (Cons x xs) = if (y == x) then True else (elem' y xs)

--mothermatch p l = (Right "") `elem` match p l
mothermatch p l = elem' (Right Nil) (match p l)

