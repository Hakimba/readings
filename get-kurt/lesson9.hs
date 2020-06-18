-- Lesson 9 -> Higher order functions
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
-- -15 ->  ((((1 - 2) - 3) - 4) - 5)

-- >>> myFoldr (-) 0 [1..5]
-- 3 -> 1 - (2 - (3 - (4 - 5)))
