-- Lesson 9 -> Higher order functions

import Data.Char as C

add3ToAll :: [Int] -> [Int]
add3ToAll [] = []
add3ToAll (x:xs) = (3+x) : add3ToAll xs

addAnA [] = []
addAnA (x:xs) = ("a"++ x) : addAnA xs

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
mymap f (x:xs) = (f x) : myMap f xs

-- >>> foldl (+) 0 [1..10]
-- 55

sumOfSquares xs = foldl (+) 0 (map (^2) xs)

-- >>> sumOfSquares [1..10]
-- 385

rcons x y = y:x
myReverse xs = foldl rcons [] xs

-- >>> myReverse [1..5]
-- [5,4,3,2,1]


myFold :: (a -> b -> a) -> a -> [b] -> a
myFold _ final [] = final
myFold f neutral (x:xs) = myFold f (f neutral x) xs

-- >>> myFold (+) 0  [1..5]
-- 15
-- >>> foldl (+) 0 [1..5]
-- 15

myFoldr f init [] = init
myFoldr f init (x:xs) = f x rightResult
  where rightResult = myFoldr f init xs

-- >>> myFold (-) 0 [1..5]
-- -15 ->  (((((0 - 1) - 2) - 3) - 4) - 5)
-- idea of the recursion pattern : function ((-) ancient val) rest

-- >>> myFoldr (-) 0 [1..5]
-- 3 -> 0 - (1 - (2 - (3 - (4 - 5))))
-- idea of the recursion pattern : (-) neutral (function val rest))

-- Q9.1

myElem x [] = False
myElem x xs = (length $  filter (== x) xs) > 0

-- >>> myElem 4 [1..3]
-- False
-- >>> myElem 4 [1..10]
-- True

-- Q9.2

isPalindrome sentence = reverse normalized == normalized
  where
    normalized = map (toLower) $ filter (/= ' ') sentence

-- >>> isPalindrome "A man a plan a canal Panama"
-- True


-- Q9.3
harmonic 1 = 1
harmonic n = 1/n + harmonic (n-1)

-- >>> harmonic 4
-- 2.083333333333333

