module Crypt (rot, derot, vigenere, devigenere, ttt) where

import Data.List
import Data.Char
import BaseLib




rot :: (Integral a) => a -> String -> String -- Chiffrement de César
rot shift = map (shiftChar shift)

derot :: (Integral a) => a -> String -> String
derot shift = rot (-shift)


--------------------------------------------------------------------------------



vigenere :: String -> String -> String
vigenere password = actualVigenere $ password2shift password

devigenere :: String -> String -> String
devigenere password = actualVigenere $ map (\x -> -x `mod` 26) $ password2shift password

password2shift :: String -> [Int]
password2shift = map (\c -> extractMaybe $ elemIndex (toLower c) ['a'..'z'])

actualVigenere :: [Int] -> String -> String
actualVigenere password = foldl (\acc x -> acc ++ [vign password acc x]) ""

vign :: [Int] -> String -> Char -> Char
vign password acc = shiftChar (password !! mod (length $ filter (`elem` alphabet) acc) (length password))


ttt = "Un troisième homme a été inculpé en Belgique pour « participation aux activités d’un groupe terroriste » dans le cadre de l’enquête sur un projet d’attentat « imminent » déjoué en France lors de l’arrestation de Reda Kriket, le 24 mars, a annoncé le parquet fédéral belge, samedi 2 avril. « Dans le cadre de l’enquête ayant conduit à l’arrestation du nommé Reda Kriket » le 24 mars en banlieue parisienne, principal suspect dans cette affaire, un juge d’instruction spécialisé en matière de terrorisme a inculpé et incarcéré vendredi « le nommé Y. A., né le 4 mai 1982, de nationalité belge », a indiqué le parquet dans un communiqué, sans autre précision. Le parquet avait déjà annoncé le 26 mars l’inculpation en Belgique de Rabah N. dans le cadre de cette affaire, distincte des attentats de Bruxelles, puis celle d’Abderamane A., arrêté à Bruxelles le 25 mars. Lire aussi :   Attentats : le réseau de Reda Kriket, un arsenal mais des cibles inconnues Véritable arsenal Jeudi 24 mars, Paris avait annoncé avoir « mis en échec un projet d’attentat en France, conduit à un stade avancé », après l’arrestation de Reda Kriket. Ce Français de 34 ans a été interpellé à Boulogne-Billancourt (Hauts-de-Seine) au terme d’une opération menée par la Direction générale de la sécurité intérieure (DGSI) et placé en garde à vue. Reda Kriket avait été condamné par contumace en juillet 2015 en Belgique dans le procès d’une filière djihadiste vers la Syrie, dont l’un des principaux prévenus n’était autre que le Belge Abdelhamid Abaaoud, l’un des organisateurs présumés des attentats du 13 novembre. Après l’arrestation de Reda Kriket, l’enquête a conduit les policiers français dans un appartement à Argenteuil, dans le Val-d’Oise. Des perquisitions ont permis de découvrir un arsenal composé d’explosifs et d’armement : cinq kalachnikovs, un fusil-mitrailleur, huit chargeurs d’AK-47, sept armes de poing, ainsi que de très nombreuses munitions. Ont également été trouvés de nombreux éléments électriques ainsi que chimiques, parmi lesquels une quantité importante d’acide et trois bouteilles d’eau oxygénée et d’acétone"
