module BaseLib (shiftChar, extractMaybe, alphabet, dist, slice) where

import Data.List

alphabet :: String
alphabet = ['a'..'z']++['A'..'Z']

shiftChar :: (Integral a) => a -> Char -> Char 
shiftChar i c
  | c `notElem` alphabet = c
  | i > 0     = shiftChar (i-1) (succAlpha c)
  | i < 0     = shiftChar (26+i) c
  | otherwise = c

succAlpha :: Char -> Char
succAlpha a
  | a == 'z' = 'a'
  | a == 'Z' = 'A'
  | otherwise = succ a

extractMaybe :: Maybe t -> t
extractMaybe (Just a) = a
extractMaybe Nothing  = error "Maybe not."

alphabetPosition :: Char -> Int
alphabetPosition a = extractMaybe $ elemIndex a alphabet

dist :: Char -> Char -> Int -- Algébrique
dist a b = alphabetPosition b - alphabetPosition a

slice :: Int -> [b] -> [[b]]
slice _ [] = []
slice n xs = (take n xs) : (slice n $ drop n xs)
