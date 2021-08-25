module Cipher where
import Data.Char

caesar :: Int -> String -> String
caesar _ "" = ""
caesar shift string = map shiftN string where shiftN = shiftChar shift

caesarInteractive :: Int -> IO String
caesarInteractive shift = do
  string <- getLine
  return $ caesar shift string


shiftChar :: Int -> Char -> Char
shiftChar shift char = chr shiftedNumChar where
  offset = if isUpper char then ord 'A' else ord 'a'
  offsetNumChar = ord char - offset
  shiftedNumChar = mod (offsetNumChar + shift) 26 + offset
