myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a list = any (\x -> x == a) list

myReverse :: [a] -> [a]
myReverse [] = []
myReverse list = lastElem : myReverse remaining
  where lastElem = last list
        remainingLength = length list - 1
        remaining = take remainingLength list

squish :: [[a]] -> [a]
--squish list = foldl (\a b -> a ++ b) [] list
squish [] = []
squish (xs:xss) = xs ++ squish xss

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ []     = error "empty list"
myMaximumBy f (x:xs) = g f x xs where
  g _ currMax [] = currMax
  g f currMax (x:xs)
    | f currMax x == GT = g f currMax xs
    | otherwise = g f x xs
