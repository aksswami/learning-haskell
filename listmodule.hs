import Data.List
import Data.Function
import Data.Char

nub' :: (Eq a, Ord a) => [a] -> [a]
nub' = uniq . sort  
		
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (x: []) 	= [x]
uniq (x:y:ys) = if x == y then uniq (y:ys) else (x: uniq (y:ys)) 

uniq' :: (Eq a) => [a] -> [a]
uniq' xs = foldr (\x acc -> if x `elem` acc then acc else x:acc) [] xs

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

revSentence :: [a] -> [a]
revSentence [] = []
revSentence (x:xs) = (revSentence xs) ++ [x]

revWord :: String -> String
revWord [] = []
revWord xs = (revSentence . takeWhile (space) $ xs) ++ if (remStr xs) == "" then "" else  " " ++ (revWord . tail . remStr $ xs)
	where space x 	= not (x `elem` [' '])


remStr ys = (dropWhile(space') ys)
	where space' x 	= not (x `elem` [' '])

revSentenceWithoutWord = revWord . revSentence

searchSublist :: (Eq a) => [a] -> [a] -> Bool
searchSublist needle haystack = 
	let nlen = length needle
	in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)