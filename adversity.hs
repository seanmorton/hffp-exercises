ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 x
  | even x = Just $ x + 2
  | otherwise = Nothing

type Name = String
type Age = Int
data Person = Person Name Age
  deriving Show
data PersonInvalid = NameEmpty
  | AgeTooLow
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name == "" = Left NameEmpty
  | age < 0 = Left AgeTooLow
  | otherwise = Right $ Person name age

notThe :: String -> Maybe String
notThe x
  | x == "the" = Nothing
  | otherwise = Just x

replaceWithA :: String -> String
replaceWithA word = case notThe word of
                     Just x -> x
                     Nothing -> "a"

replaceThe :: String -> String
replaceThe string = replacedTheString
  where stringWords = words string
        replacedWords = fmap replaceWithA stringWords
        replacedTheString = unwords replacedWords

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord input = if (isValidWord) then Just (Word' input) else Nothing
  where isValidWord = numConsonants >= numVowels
        numConsonants = length input - numVowels
        numVowels = sum $ map isVowel input
        isVowel = \c -> if (elem c vowels) then 1 else 0


isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee defaultVal _ Nothing = defaultVal
mayybee defaultVal f (Just a) = f a

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (m:ms) =
  case m of
   Just a -> a : catMaybes ms
   Nothing -> catMaybes ms

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe maybes = if (length values > 0) then Just values else Nothing
  where values = catMaybes maybes

lefts' :: [Either a b] -> [a]
lefts' = foldr collectLefts []
  where collectLefts = \elem acc ->
          case elem of
            Left a -> a : acc
            _ -> acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr partition ([], [])
  where partition = \elem acc ->
          case elem of
            Left l -> ((l : fst acc), (snd acc))
            Right r -> ((fst acc), (r : snd acc))

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f e =  either' (\_ -> Nothing) (\r -> Just $ f r) e

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ f (Right x) = f x
