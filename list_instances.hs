{-# LANGUAGE NoImplicitPrelude #-}

import Prelude (
    Applicative,
    Functor,
    Int,
    Monad,
    Monoid,
    Semigroup,
    Show,
    fmap,
    mappend,
    mempty,
    pure,
    ($),
    (*),
    (+),
    (<*>),
    (<>),
    (>>=),
 )

data List a = Empty | Cons a (List a)
    deriving (Show)

instance Semigroup (List a) where
    (<>) = append

instance Monoid (List a) where
    mempty = Empty

instance Functor List where
    fmap = map

instance Applicative List where
    pure x = Cons x Empty
    (<*>) Empty _ = Empty
    (<*>) (Cons f fs) xs = mappend (fmap f xs) (fs <*> xs)

instance Monad List where
    (>>=) list f = concat $ fmap f list

infixr 6 <+>
(<+>) :: a -> List a -> List a
(<+>) = Cons

append :: List a -> List a -> List a
append Empty list2 = list2
append (Cons x xs) list2 = x <+> append xs list2

length :: List a -> Int
length Empty = 0
length (Cons _ xs) = 1 + length xs

sum :: List Int -> Int
sum Empty = 0
sum (Cons x xs) = x + sum xs

product :: List Int -> Int
product Empty = 1
product (Cons x xs) = x * product xs

foldr :: (a -> b -> b) -> b -> List a -> b
foldr _ baseVal Empty = baseVal
foldr f baseVal (Cons x xs) = f x (foldr f baseVal xs)

foldl :: (b -> a -> b) -> b -> List a -> b
foldl _ baseVal Empty = baseVal
foldl f baseVal (Cons x xs) = foldl f (f baseVal x) xs

flip :: (a -> b -> c) -> b -> a -> c
flip f b a = f a b

reverse :: List a -> List a
reverse = foldl (flip Cons) Empty

map :: (a -> b) -> List a -> List b
map _ Empty = Empty
map f (Cons x xs) = Cons (f x) (map f xs)

concat :: List (List a) -> List a
concat = foldr append Empty

l1 = 1 <+> 2 <+> 3 <+> Empty
