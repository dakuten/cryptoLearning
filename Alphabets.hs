module Alphabets 

(
  Alphabet(Lowercase, Uppercase, Alpha, Alphanumeric, ASCII),
  mkExotic,
  lengthAlphabet,
  getAlphabet,
  mvAlpha,
  succAlpha, precAlpha,
  toVector, toString, toChar,
  toInt
)

where

import Data.List
import Data.Char
import Data.Maybe

data Alphabet = Lowercase
              | Uppercase
              | Alpha
              | Alphanumeric
              | ASCII
              | Exotic String
              deriving (Show)

mkExotic :: String -> Alphabet
mkExotic str = Exotic $ epurated str
              where
                epurated :: String -> String
                epurated []  = []
                epurated [c] = [c]
                epurated (c:cs) = c:(epurated $ filter (/=c) cs)


getAlphabet :: Alphabet -> String
getAlphabet Lowercase    = ['a'..'z']
getAlphabet Uppercase    = ['A'..'Z']
getAlphabet Alpha        = ['a'..'z']++['A'..'Z']
getAlphabet Alphanumeric = ['0'..'9']++['a'..'z']++['A'..'Z']
getAlphabet ASCII        = map chr [0..0x7F]
getAlphabet (Exotic a)   = a 

lengthAlphabet :: Alphabet -> Int
lengthAlphabet = length . getAlphabet

mvAlpha :: Int -> Alphabet -> Char -> Maybe Char
mvAlpha i a c = case elemIndex c alpha of
                  Just j  -> Just $ alpha !! ((j+i) `mod` (length alpha))
                  Nothing -> Nothing

                where alpha = getAlphabet a

succAlpha = mvAlpha 1
precAlpha = mvAlpha (-1)

toInt :: Alphabet -> Char -> Maybe Int
toInt a c = elemIndex c alpha where alpha = getAlphabet a

isValid :: Alphabet -> String -> Bool
isValid a s = and $ map (`elem` (getAlphabet a)) s


toVector :: Alphabet -> String -> Maybe [Int]
toVector a s = if any (==Nothing) mInt
                then Nothing
                else Just $ catMaybes mInt
             where mInt = map (toInt a) s

toString :: Alphabet -> [Int] -> String
toString a = map (toChar a)

toChar :: Alphabet -> Int -> Char
toChar a = (!!) (getAlphabet a)

dist :: Alphabet -> Char -> Char -> Int -- Fausse distance 
dist alpha a b = (pos b) - (pos a)
       where pos = fromJust.(toInt alpha)
