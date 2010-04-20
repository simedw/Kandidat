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
 deriving Show


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

mothermatch p l = case match p l of
    { Success xs  -> empty xs
    ; _           -> False
    }

match :: Eq a => Regexp a -> List a -> Res a
match r l = case r of
    { Empty -> Success l
    ; Symbol a -> case l of
        Cons x xs -> case (x==a) of
                      { True  -> Success xs
                      ; False -> Failed
                      }
        Nil       -> Failed
    ; Star p -> case match p l of
                { Failed -> Success l       -- no match
                ; Success xs -> case xs of  -- one match
                    { Nil -> Success Nil
                    ; xs  -> match (Star p) xs
                    }
                }
    ; Union p q -> case match p l of
            { Failed -> match q l
            ; x      -> match q l -- x
            }
    ; Seq p q -> case match p l of
            { Failed -> Failed
            ; Success xs -> match q xs
            }
    }

-- parse :: String -> [Regexp Char] -> Regexp Char
parse cs stack = case cs of
    { Cons ',' rest  -> parse rest (Empty : stack)
    ; Cons '*' rest  -> parse rest (Star (head stack) : tail stack)
    ; Cons '.' rest  -> parse rest (Seq (head $ tail stack) (head stack) : drop 2 stack)
    ; Cons '|' rest  -> parse rest (Union (head $ tail stack) (head stack) : drop 2 stack)
    ; Cons c   rest  -> parse rest (Symbol c : stack)
    ; Nil       -> head stack 
    }


