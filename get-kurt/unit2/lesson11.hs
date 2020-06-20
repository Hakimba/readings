-- Lesson 11 -> Type basics

-- Quick check 11.4
-- Because the function passed as argument could transform the type of the list which be mapped

-- myMap show [1..4] :: (a -> String) -> [Int] -> [String]

-- Q11.1
-- filter :: (a -> Bool) -> [a] -> [a]

-- Q11.2
myHead :: [a] -> [a]
myHead [] = []
myHead (x:xs) = [x]

-- idem for tail

-- Q11.3

-- myFoldl :: (a -> b -> a) -> a -> [b] -> a
