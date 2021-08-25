import Control.Applicative

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

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name age weight =
    Cow <$> noEmpty name <*> noNegative age <*> noNegative weight

cowFromString' :: String -> Int -> Int -> Maybe Cow
cowFromString' name age weight =
    liftA3 Cow (noEmpty name) (noNegative age) (noNegative weight)

-- Exercises

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) (Identity a) = Identity $ f a

data Pair a = Pair a a deriving (Show)
instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
    pure a = Pair a a
    (<*>) (Pair f g) (Pair i j) = Pair (f i) (g j)

data Two a b = Two a b deriving (Show)
instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
    pure = Two mempty

  -- Test each of these against the applicative laws
    (<*>) (Two x f) (Two y b) = Two (x <> y) (f b)
  --(<*>) (Two _ f) (Two x b) = Two x (f b)

data Three a b c = Three a b c deriving (Show)
instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure = Three mempty mempty
    (<*>) (Three f f' f'') (Three g g' g'') = Three (f <> g) (f' <> g') (f'' g'')

--
--data Three' a b = Three' a b b deriving (Show)
--instance Functor (Three' a) where
--    fmap f (Three' a b c) = Three' a (f b) (f c)
--
--data Four a b c d = Four a b c d deriving (Show)
--instance Functor (Four a b c) where
--    fmap f (Four a b c d) = Four a b c (f d)
--
--data Four' a b = Four' a a a b deriving (Show)
--instance Functor (Four' a) where
--    fmap f (Four' a b c d) = Four' a b c (f d)
