module Math

(
 (<+>),
 (*>),
 modL,
 permute,
 permutations,
 permuteW,
 inversePerm,
 slice,
 inverseZnZ,
 inverseMZnZ,
 pairs,
 gcdAll,
 mean,
 subwords
)

where

import Data.List
import Data.Maybe
import Data.Matrix
import Prelude hiding ((*>))


type Permutation a = ([a],[a])

(<+>) :: Num a => [a] -> [a] -> [a]
(<+>)  = zipWith (+)

(*>) :: Num a => a -> [a] -> [a]
(*>) a = map (*a)

modL :: [Int] -> Int -> [Int]
modL ls i = map (`mod` i) ls

permute :: Eq a => Permutation a -> a -> a -- unsafe
permute (from, to) e = case elemIndex e from of
                        Just i -> to !! i
                        Nothing -> error "Element not in permutation"

permuteW :: Eq a => [Int] -> [a] -> [a]
permuteW perm w = map ((!!) w) perm

inversePerm :: [Int] -> [Int]
inversePerm perm = map (fst) $ sortBy (\(_,x) (_,y) -> compare x y) $ zip [0..length perm -1] perm


slice :: Int -> [a] -> [[a]]
slice _ [] = []
slice n xs = take n xs : slice n (drop n xs)

inverseZnZ :: Int -> Int -> Int -- trouve l'inverse de x dans (Z/pZ,+,*)
inverseZnZ n x = (snd (eucl n x)) `mod` n

eucl :: Int -> Int -> (Int, Int)
eucl a b = eucl' a 1 0 b 0 1
            where
              eucl' r u v 0 _ _    = (u,v)
              eucl' r u v r' u' v' = eucl' r' u' v' 
                                          (r-(r `div` r')*r')
                                          (u-(r `div` r')*u')
                                          (v-(r `div` r')*v')


comatrix :: Num a => Matrix a -> Matrix a
comatrix m = matrix (nrows m) (ncols m) (\(i,j) -> ((-1)^(i+j)) * (detLaplace (minorMatrix i j m)))

inverseMZnZ :: Int -> Matrix Int -> Matrix Int
inverseMZnZ n m = Data.Matrix.transpose $ fmap (`mod` n) $ scaleMatrix (inverseZnZ n ((detLaplace m) `mod` n)) (comatrix m)

buildCartesian :: [a] -> [b] -> [(a,b)] -- inefficient, not lazy
buildCartesian _ [] = []
buildCartesian [] _ = []
buildCartesian (x:xs) ys = (map (\y -> (x,y)) ys) ++ (buildCartesian xs ys)

pairs :: Eq a => [a] -> [(a,a)]
pairs xs = concat $ map (\x-> zip (repeat x) (delete x xs)) xs

gcdAll :: Integral a => [a] -> a
gcdAll [] = -1
gcdAll xs = foldl1 gcd xs

mean :: Fractional a => [a] -> a
mean xs = sum xs / (fromIntegral $ length xs)

subwords :: Int -> [a] -> [[a]]
subwords n ls@(_:xs)
     | n > length ls = []
     | otherwise     =  (take n ls) : (subwords n xs)

