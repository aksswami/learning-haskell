doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
						then x
						else x * 2
						
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _<-xs]

removeNonUpperCase :: [Char] -> String
removeNonUpperCase st = [c | c <- st, c `elem` ['A'..'Z']]

nestedList xxs = [[x | x <- xs, odd x] | xs <- xxs]

rightTriangle = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a + b + c == 24]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number seven"
lucky x = "Sorry, yo're out of luck pal"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' a b = (fst a + fst b, snd a + snd b)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "Can't call head on empty list, dummy!"
head' (x:_) = x

head'' :: [a] -> a
head'' xs = case xs of [] -> error "No head of empty list"
                       (x:_) -> x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two element: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length'' :: (Num b) => [a] -> b
length'' [] = 0
length'' (_:xs) = 1 + length'' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital' :: String -> String
capital' "" = "Empty String, whoops!"
capital' all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
	| bmi <= skinny 	= "You're underweight, you emo, you!"
	| bmi <= normal 	= "You're supposedly normal. Pfft, I bet you're ugly!"
	| bmi <= fat	 	= "You're fat! Lose some weight, fatty!"
	| otherwise			= "You're a whale, congratulation!"
	where bmi = weight / height ^ 2
	      (skinny, normal, fat) = (18.5, 25.0, 30.0)
		  
calBmis :: (RealFloat a) => [(a,a)] -> [a]
calBmis xs = [bmi w h | (w,h) <- xs]
	 where bmi weight height = weight / height ^ 2
		  
max' :: (Ord a) => a -> a -> a
max' a b
	| a > b 	= a
	| otherwise = b
	
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
	| a > b 	= GT
	| a == b 	= EQ
	| otherwise = LT
	
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
	where (f:_) = firstname
	      (l:_) = lastname
		  
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
	let sideArea = 2 * pi * r * h
	    topArea = pi * r ^ 2
	in  sideArea + 2 * topArea
	
calBmis' :: (RealFloat a) => [(a, a)] -> [a]
calBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
	where what [] = "empty."
	      what [x] = "a singleton."
	      what xs = "a longer list."                                               

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of an empty list"
maximum' [x] = x
maximum' (x:xs)
	| x > maxTail 	= x
	| otherwise		= maxTail
	where maxTail = maximum' xs

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "maximum of an empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
	| n <= 0 	= []
	| otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
	| n <= 0 	= []
take' _ [] 		= []
take' n (x:xs)	= x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs) 
	| a == x 	= True
	| otherwise	= a `elem'` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
	let smallerSorted 	= quicksort [a | a <- xs, a <= x]
	    biggerSorted	= quicksort [a | a <- xs, a > x]
	in smallerSorted ++ [x] ++ biggerSorted

multiThree :: (Num a) => a -> a -> a -> a
multiThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) =>  a -> Ordering
compareWithHundred x = compare 100 x

compareHundred :: (Num a, Ord a) => a -> Ordering
compareHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g 
	where g x y = f y x

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
	| p x 		= x : filter' p xs
	| otherwise	= filter' p xs

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999..])
	where p x = x `mod` 3829 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
	| even n 	= n: chain (n `div` 2)
	| odd n 	= n: chain (n*3 +1) 

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
	where isLong xs = length xs > 15

numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

sumFold :: (Num a) => [a] -> a
sumFold xs = foldl (\acc x -> acc + x) 0 xs

elemFold :: (Eq a) => a -> [a] -> Bool
elemFold y ys = foldl (\acc x -> if x == y then True else acc) False ys

elemFold' :: (Eq a) => a -> [a] -> Bool
elemFold' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x: acc) [] xs

mapLeftFold :: (a -> b) -> [a] -> [b]
mapLeftFold f xs = foldl (\acc x -> acc ++ [f x]) [] xs

maxFold :: (Ord a) => [a] -> a
maxFold = foldl1 (\acc x -> if x > acc then x else acc) 

reverseFold :: [a] -> [a]
reverseFold = foldl (\acc x -> x: acc) []

productFold :: (Num a) => [a] -> a
productFold = foldl1 (*)

filterFold :: (a -> Bool) -> [a] -> [a]
filterFold p = foldr (\x acc -> if p x then x : acc else acc) []

headFold :: [a] -> a
headFold = foldr1 (\x _ -> x)

lastFold :: [a] -> a
lastFold = foldl1 (\_ x -> x)

sqrtSums :: Int
sqrtSums = length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1..]))) + 1
 