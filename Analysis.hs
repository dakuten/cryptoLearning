module Analysis

(
  analysisCiphertext,
  CipherType(..),
  occurTable,
  FreqTable(..)
)

where

import Alphabets
import qualified RawData as R
import Data.List
import Ciphers
import Data.Maybe
import Data.Matrix
import Math

data FreqTable a = FreqTable {
                               alpha :: Alphabet,
                               table :: [(Char, a)]
                             } deriving (Show)

freqEN :: FreqTable Float
freqEN = FreqTable Lowercase R.freqEN


occurTable :: Alphabet -> String -> FreqTable Int
occurTable a text = FreqTable a $ buildOccur (getAlphabet a) text
            where buildOccur [] _ = []
                  buildOccur xs [] = zip xs $ repeat 0
                  buildOccur (x:xs) text = (x,length $ filter (==x) text):(buildOccur xs $ filter (/=x) text)

sortFreq :: Ord a => FreqTable a -> FreqTable a
sortFreq (FreqTable a t) = FreqTable a $ reverse $ sortOn (\(_,f) -> f) t

match :: (Ord a, Ord b) => FreqTable a -> FreqTable b -> [(Char, Char)]
match ft0 ft1 = zip (extract ft0) (extract ft1)
                where extract ft = map fst $ table $ sortFreq ft

data CipherType = ROT
                | Vigenere
                | Substitution
                | Affine
                | Hill
                | Permutation
                deriving (Show, Eq)


analysisCiphertext :: Alphabet -> CipherType -> String -> [Cipher] -- Maybe [Cipher]

analysisCiphertext alpha ROT text = catMaybes $ map (mkROT alpha) possibleKeys 
       where sortedByProbability = (match (occurTable alpha text) freqEN) 
             possibleKeys = map
                             (\(a, _) -> tC ((tI a - tI (snd (sortedByProbability !! 0))) `mod` m))
                             sortedByProbability 
             strAlpha = getAlphabet alpha
             m        = lengthAlphabet alpha
             tI       = fromJust.(toInt alpha)
             tC       = (!!) strAlpha

analysisCiphertext alpha Affine text = catMaybes $ map (\(pp,oo) -> mkAffine alpha pp oo) validKeys 
        where 
          freqLetters  = map fst $ table $ sortFreq $ occurTable alpha text 
          possibleKeys = map (\(x,y) -> (tI x, tI y)) $ pairs freqLetters
          keys    = map (\(x,y) ->
                        toList $  fmap (`mod` lA) $ (inverseMZnZ lA baseM)*(fromList 2 1 [x, y])
                        ) possibleKeys
          validKeys = map (\(a:b:_) -> (tC a, tC b)) $ filter (\(x:_) -> gcd x lA == 1) keys
          (occ1:occ2:_) = map fst $ table $ sortFreq $ freqEN
          lA = lengthAlphabet alpha
          baseM = fromList 2 2 [tI occ1, 1, tI occ2, 1]
          tI = fromJust.(toInt alpha)
          tC = toChar alpha

--analysis alpha Substitution text =
--Not today keshi qing kan zher : http://practicalcryptography.com/cryptanalysis/letter-frequencies-various-languages/english-letter-frequencies/


analysisCiphertext alpha Vigenere text = catMaybes $ map (mkVigenere alpha) $ keywords alpha text
          where 
            possLengthD []        = []
            possLengthD [x]       = []
            possLengthD ls@(x:xs) = (f $ elemIndices x ls) : possLengthD xs
              where f list = gcdAll $ map (\(a,b) -> abs $ (a-b)) $ pairs list

            indexCoincidence :: Fractional a => Alphabet -> String -> a
            indexCoincidence alpha text = (fromIntegral $ sum $ map ((\f -> f*(f-1)).(\a -> length $ filter (==a) text)) (getAlphabet alpha)) / (fromIntegral  $ (length text)*(length text -1))

            refIndex :: Fractional a => FreqTable a -> a
            refIndex freq = sum $ map ((^2).snd) $ table freq

            iCVign :: Fractional a => Alphabet -> Int -> String -> [a]
            iCVign alpha n txt = map (indexCoincidence alpha) $ Data.List.transpose $ slice n txt

            iCValid :: (Ord a, Fractional a) => FreqTable a -> a -> Bool
            iCValid freq ic = abs (refIndex freq - ic) < 0.01

            actualLengthD alpha txt = sort $ filter (\l->iCValid freqEN (mean $ iCVign alpha l txt)) $ delete (-1) $ nub $ possLengthD $ subwords 3 txt 

            keywords alpha txt = map (map (\xs -> fst (xs !! 0))) $ map (map (sortOn (\(_,f) -> abs (refIndex freqEN -f)))) $ map (map (\s -> [mG i s | i<-[0..lengthAlphabet alpha -1]])) $ map trspd lengths
                where trspd   = (Data.List.transpose).((flip slice) txt)
                      lengths = actualLengthD alpha txt
                      mG n s  = (toChar alpha n, sum [ (snd (tFEN !! i))*(fromIntegral f)/(fromIntegral $ length s) 
                                | i <- [0..lengthAlphabet alpha -1],
                                  let f = (length $ filter (==(toChar alpha $ (n+i)`mod`(lengthAlphabet alpha))) s )])
                      tFEN    = table freqEN


analysisCiphertext alpha Hill text = error "Not implemented"





analysisPlaintext :: Alphabet -> CipherType -> String -> String -> Cipher
analysisPlaintext alpha Hill ciphtxt plaintxt = error "Not yet implemented" 
