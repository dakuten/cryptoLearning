module ROTAnalysis (rotDecrypt) where

import Data.Char
import Data.List
import Crypt
import BaseLib

frequency :: [(Char, Double)]
frequency = zip ['a'..'z']
                [7.636,0.901,3.260,3.669,14.715,1.066,0.866,0.737,7.529,0.545,0.049,5.456,2.968,7.095,5.378,3.021,1.362,6.553,7.648,7.244,6.311,1.628,0.114,0.387,0.308,0.136]

textCount :: String -> [(Char, Integer)]
textCount = foldl addTo (zip ['a'..'z'] $ replicate 26 0)
            where addTo [] _ = []
                  addTo (y:ys) x = if toLower x == fst y then (fst y, snd y +1):ys else y: addTo ys x

textFreq :: String -> [(Char, Double)]
textFreq text = map (\(c,n)-> (c, 100*fromIntegral n / fromIntegral sz)) count
                  where count = textCount text
                        sz = sum $ snd $ unzip count
sortedFreq :: String -> [(Char, Double)]
sortedFreq text = sortOn (\(_,n) -> -n) $ textFreq text

newE :: String -> Char
newE text = fst $ maximumBy (\(_,x) (_,y) -> compare x y) $ textCount text

rotDecrypt :: String -> String
rotDecrypt text = rot (-(dist 'e' $ newE text)) text
