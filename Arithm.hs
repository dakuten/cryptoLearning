module Arithm (primeFactors, factors) where


import Data.List

primes :: (Integral a) => [a]
primes = sieve [2..]
        where sieve (p:ps) = p:(sieve $ filter (\x-> x `mod` p /= 0) ps)

_primeFactors :: (Integral a) => a -> [a] -> [a]
_primeFactors _ [] = []
_primeFactors (-1) _ = [-1]
_primeFactors 1 _ = []
_primeFactors n (p:ps) = if n `mod` p == 0 then p:_primeFactors (n `div` p) (p:ps) else _primeFactors n ps

primeFactors :: (Integral a) => a -> [a]
primeFactors n = _primeFactors n primes

factors :: (Integral a) => a -> [a]
factors n = sort.nub $ map product $ filter (not.null) $ subsequences $ primeFactors n
