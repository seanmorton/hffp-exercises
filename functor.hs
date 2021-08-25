data TestFunc a = Nope | Yes a
    deriving (Eq, Show)

instance Functor TestFunc where
    fmap _ Nope = Nope
    fmap f (Yes a) = Yes (f a)

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (.) :: ((m -> n) -> f m -> f n) -> ((x -> y) -> g x -> g y) -> (x -> y) -> f m
-- fmap :: Functor f => (m -> n) -> f m -> f n
-- fmap :: Functor g => (x -> y) -> g x -> g y
--

d = fmap ((return '1' ++) . show) (\x -> [x, 1 .. 3])

newtype Identity a = Identity a deriving (Show)
instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

data Pair a = Pair a a deriving (Show)
instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

data Two a b = Two a b deriving (Show)
instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

data Three a b c = Three a b c deriving (Show)
instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

data Three' a b = Three' a b b deriving (Show)
instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

data Four a b c d = Four a b c d deriving (Show)
instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

data Four' a b = Four' a a a b deriving (Show)
instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)
instance Functor Possibly where
    fmap f (Yeppers a) = Yeppers (f a)
    fmap _ LolNope = LolNope

data Sum a b = First a | Second b deriving (Eq, Show)
instance Functor (Sum a) where
    fmap f (Second b) = Second (f b)
    fmap _ (First a) = First a

getInt :: IO Int
getInt = read <$> getLine

data Quant a b
    = Finance
    | Desk a
    | Bloor b
    deriving (Show)
instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)

data Notorious g o a t = Notorious (g o) (g a) (g t)
    deriving (Show)

instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (f <$> gt)
