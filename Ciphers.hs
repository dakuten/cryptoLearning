module Ciphers

(
  Cipher(Cipher),
  --Method(ROT, Vigenere, Substitution, Affine, Hill, Permutation),
  mkROT, mkVigenere, mkSubstitution, mkAffine, mkHill, mkPermutation,
  keyROT,
  crypt,
  decrypt,
  isValid
)

where

import Alphabets
import Math
import Data.Maybe
import Data.Matrix
import Data.List(sort)
import Prelude hiding ((*>))
--import Numeric.Matrix hiding (map)

data Cipher = Cipher Alphabet Method deriving (Show)
data Method = ROT Char
            | Vigenere String
            | Substitution String
            | Affine (Char, Char)
            | Hill String
            | Permutation [Int]
            deriving (Show)

returnIfValid :: Cipher -> Maybe Cipher
returnIfValid c = if isValid c then Just c else Nothing

mkROT :: Alphabet -> Char -> Maybe Cipher
mkROT a c = returnIfValid ciph
          where meth = ROT c
                ciph = Cipher a meth

mkVigenere :: Alphabet -> String -> Maybe Cipher
mkVigenere a pswd = returnIfValid ciph
                    where meth = Vigenere pswd
                          ciph = Cipher a meth

mkSubstitution :: Alphabet -> String -> Maybe Cipher
mkSubstitution a s = returnIfValid (Cipher a (Substitution s))

mkAffine :: Alphabet -> Char -> Char -> Maybe Cipher
mkAffine alpha a b = returnIfValid (Cipher alpha (Affine (a, b)))

mkHill :: Alphabet -> String -> Maybe Cipher
mkHill a key = returnIfValid (Cipher a (Hill key))

mkPermutation :: Alphabet -> [Int] -> Maybe Cipher
mkPermutation a xs = returnIfValid (Cipher a (Permutation xs))

keyROT :: Cipher -> Char
keyROT (Cipher _ (ROT k)) = k
keyROT _                  = error "The cipher is not a ROT"

crypt :: Cipher -> String -> String
crypt _ [] = []
crypt (Cipher a (ROT k)) s = crypt (Cipher a $ Vigenere [k])  s
crypt (Cipher a (Vigenere key)) text = toString a $ (tVA text) <+> (concat.repeat $ tVA key) `modL` lAlpha
                                        where
                                          lAlpha = lengthAlphabet a
                                          tVA = fromJust . (toVector a)
crypt (Cipher a (Substitution perm)) text = map (permute (getAlphabet a, perm)) text
crypt (Cipher a (Affine (aa, bb))) text = toString a $ ((tIA aa)*>(tVA text)) <+> (repeat$tIA bb) `modL` lengthAlphabet a
                                        where
                                          tIA = fromJust.(toInt a)
                                          tVA  = fromJust.(toVector a)
crypt (Cipher a (Hill pscd)) text = toString a $ modL (toList $ q*m) (lengthAlphabet a)
                                    where
                                      m = toMatrix a pscd
                                      n = nrows m
                                      q = fromList n n $ fromJust $ toVector a text
crypt (Cipher _ (Permutation perm)) text = concat $ map (permuteW perm) $ slice (length perm) text



decrypt :: Cipher -> String -> String
decrypt _ [] = []
decrypt (Cipher a (ROT k)) s = decrypt (Cipher a $ Vigenere [k]) s
decrypt (Cipher a (Vigenere key)) s = crypt (Cipher a (Vigenere (toString a $ (map negate $ fromJust $ toVector a key) `modL` lengthAlphabet a))) s
decrypt (Cipher a (Substitution perm)) text = map (permute (perm, getAlphabet a)) text
decrypt (Cipher a (Affine (aa, bb))) text = toString a $ (inverseZnZ (lengthAlphabet a) $ tIA aa)*>(tVA text <+> (repeat $ negate $ tIA bb)) `modL` lengthAlphabet a
                                        where
                                          tIA = fromJust.(toInt a)
                                          tVA  = fromJust.(toVector a)
decrypt (Cipher a (Permutation perm)) text = crypt (Cipher a (Permutation (inversePerm perm))) text
decrypt (Cipher a (Hill pscd)) text = toString a $ modL (toList $ q*m) (lengthAlphabet a)
                                    where
                                      m = inverseMZnZ (lengthAlphabet a) $ toMatrix a pscd
                                      n = nrows m
                                      q = fromList n n $ fromJust $ toVector a text


isValid :: Cipher -> Bool
isValid (Cipher a (ROT c)) = c `elem` (getAlphabet a)
isValid (Cipher a (Vigenere key)) = and $ map (`elem` getAlphabet a) key
isValid (Cipher a (Substitution perm)) = perm `elem` (permutations $ getAlphabet a)
isValid (Cipher a (Affine (aa, bb)))      = aa `elem` alpha && bb `elem` alpha && gcd (fromJust $ toInt a aa) (length alpha) == 1
                                        where alpha = getAlphabet a
isValid (Cipher a (Hill pscd)) = gcd (detLaplace $ toMatrix a pscd) (lengthAlphabet a) == 1
isValid (Cipher _ (Permutation xs)) = (sort xs) == [0..length xs -1]

toMatrix :: Alphabet -> String -> Matrix Int
toMatrix a k = fromList n n vectK
              where
                n     = (round.sqrt.fromIntegral.length) k
                vectK = fromJust $ toVector a k
