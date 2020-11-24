-- file: ch03/ListADT.hs

data List a = Cons a (List a)
    | Nil
    deriving (Show)

toList (Cons x xs) = x:(toList xs)
toList Nil = []
