module Chapter15 where

import Data.Monoid
import Test.QuickCheck

data Optional a =
  Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) (Only x) (Only y) = Only(x <> y)
  (<>) (Only x) Nada = Only x
  (<>) Nada (Only y) = Only y
  (<>) _ _ = Nada

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) x@First'{getFirst' = Only _} _ = x
  (<>) _ y = y

instance Monoid (First' a) where
  mempty = First' Nada

data Two a b = Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two x y) (Two x' y') = Two (x <> x') (y <> y')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TwoType a b = Two (a b) -> Two (a b) -> Two (a b) -> Bool
