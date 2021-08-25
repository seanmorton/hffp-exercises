{-# LANGUAGE InstanceSigs #-}

import Control.Applicative
import Control.Monad

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
        then [x * x, x * x]
        else [x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
    xs >>= \x ->
        if even x
            then [x * x, x * x]
            else [x]

data Cow = Cow
    { name :: String
    , age :: Int
    , weight :: Int
    }
    deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
    | n >= 0 = Just n
    | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck cow =
    let w = weight cow
        n = name cow
     in if n == "Bess" && w > 499
            then Nothing
            else Just cow

mkCow :: String -> Int -> Int -> Maybe Cow
mkCow name age weight = do
    validName <- noEmpty name
    validAge <- noNegative age
    validWeight <- noNegative weight
    weightCheck $ Cow validName validAge validWeight

mkCow' :: String -> Int -> Int -> Maybe Cow
mkCow' name age weight = do
    noEmpty name
        >>= \validName ->
            noNegative age
                >>= \validAge ->
                    noNegative weight
                        >>= \validWeight ->
                            weightCheck $ Cow validName validAge validWeight

-- years ago
type Founded = Int

-- number of programmers
type Coders = Int

data SoftwareShop = Shop
    { founded :: Founded
    , programmers :: Coders
    }
    deriving (Eq, Show)

data FoundedError
    = NegativeYears Founded
    | TooManyYears Founded
    | NegativeCoders Coders
    | TooManyCoders Coders
    deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded foundedAgo
    | foundedAgo < 0 = Left $ NegativeYears foundedAgo
    | foundedAgo > 500 = Left $ TooManyYears foundedAgo
    | otherwise = Right foundedAgo

validateCoders :: Int -> Either FoundedError Coders
validateCoders coders
    | coders < 0 = Left $ NegativeCoders coders
    | coders > 5000 = Left $ TooManyCoders coders
    | otherwise = Right coders

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
    validYears <- validateFounded years
    validCoders <- validateCoders coders
    Right $ Shop validYears validCoders

data Either' a b = Bad a | Good b deriving (Eq, Show)

instance Functor (Either' a) where
    fmap _ (Bad y) = Bad y
    fmap f (Good x) = Good (f x)

instance Applicative (Either' a) where
    pure x = Good x
    (<*>) (Bad y) _ = Bad y
    (<*>) (Good f) either = fmap f either

instance Monad (Either' a) where
    return = pure
    (>>=) (Bad y) _ = Bad y
    (>>=) (Good x) f = f x

data Nope a = NopeDotJpg

instance Functor Nope where
    fmap :: (a -> b) -> Nope a -> Nope b
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
    NopeDotJpg >>= _ = NopeDotJpg

j :: Monad m => m (m a) -> m a
j = join

--j = (>>= id)

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftA2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)
