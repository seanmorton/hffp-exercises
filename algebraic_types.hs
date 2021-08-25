{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

data Price = Price Integer
  deriving (Eq, Show)

data Size = Size Integer
  deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata
  deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle =
  Car Manufacturer Price
  | Plane Airline Size
  deriving (Eq, Show)


myCar = Car Mini $ Price 14000
urCar = Car Mazda $ Price 20000
clownCar = Car Tata $ Price 7000
doge = Plane PapuAir $ Size 10

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu _ = error "Not a car!"

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int,Int) where
  tooMany (a, b) = (+) a b > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)
newtype Goatss = Goatss (Int,Int) deriving (Eq, Show, TooMany)

data Name = Name String deriving Show
data Acres = Acres Int deriving Show
data FarmerType = DairyFarmer | WheatFarmer | CattleFarmer deriving Show
data FarmerRec =
  FarmerRec { name :: Name
            , acres :: Acres
            , farmerType :: FarmerType }
            deriving Show

isDairyFarmer :: FarmerRec -> Bool
isDairyFarmer farmer =
  case farmerType farmer of
    DairyFarmer -> True
    _ -> False

data Weekday =
    Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  deriving Show


isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf __ [] = False
isSubseqOf list@(x:xs) (y:ys)
  | x == y = isSubseqOf xs ys
  | otherwise = isSubseqOf list ys

