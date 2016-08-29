
import           Arithm
import           BaseLib
import           Crypt
import           Data.Char
import           Data.List
import ROTAnalysis

findTriplets :: String -> [(String, [Int])]
findTriplets text = snd $ foldl _iterateOverTriplets (("", 0), [])
                          (filter (`elem` alphabet) $ map toLower text)


_iterateOverTriplets :: ((String, Int), [(String, [Int])]) -> Char -> ((String, Int), [(String, [Int])])
_iterateOverTriplets (([],      n), lst) x = (([x],   n), lst)
_iterateOverTriplets (([a],     n), lst) x = (([a,x], n), lst)
_iterateOverTriplets (([a,b],   n), lst) x = _constructTriplets [a,b,x] n lst
_iterateOverTriplets (([a,b,c], n), lst) x = _constructTriplets [b,c,x] n lst

_constructTriplets ::  String -> Int -> [(String, [Int])] -> ((String, Int), [(String, [Int])])
_constructTriplets w n lst = ((w, n+1),
                              if w `elem` fst (unzip lst)
                                then map (
                                        \t@(triplet, xs) ->
                                        if triplet == w
                                        then (triplet, n:xs)
                                        else t
                                       )
                                   lst
                              else (w,[n]):lst)

repeatingTriplets :: String -> [(String, [Int])]
repeatingTriplets text = filter (\(t, xs) -> length xs >= 2) $ findTriplets text

_distancesOfTriplets :: String -> [(String, [Int])]
_distancesOfTriplets text = map (\(t, xs) -> (t,
                                map (\[x,y] -> x - y)
                                    (filter ((==2).length) $ subsequences xs)
                               ) )
                         $ repeatingTriplets text

distancesOfTriplets text = concatMap snd $ _distancesOfTriplets text

possibleLengths :: String -> [(Int, Int)]
possibleLengths text = map (\l@(x:xs) -> (x, length l)) $ group $ sort pLengths
                        where pLengths = concatMap factors $ distancesOfTriplets text

minLength :: [(Int, Int)] -> [(Int, Int)] -- Liste doit être ordonnée par la première coordonée
minLength [] = []
minLength ((n, eff):xs) = (n, minimum (map (\x -> eff - x)  $ 0:(snd.unzip $ filter (\(x,_)-> x `mod` n == 0) xs))):minLength xs

guessKeyLength text = fst $ maximumBy (\(_,x) (_,y) -> compare x y) $ minLength $ possibleLengths text

vignDecrypt n text =  concat $ transpose $ map rotDecrypt $ transpose $ slice n pure
                      where pure = filter (`elem` alphabet) $ map toLower text

vigenereDecrypt text = vignDecrypt (guessKeyLength text) text
