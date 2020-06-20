-- Quick check 8.1

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- >>> myLength []
-- 0
-- >>> myLength [1..5]
-- 5

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake _ [] = []
myTake n (x:xs) = x : myTake (n-1) xs

-- >>> myTake 5 [1..3]
-- [1,2,3]
-- >>> myTake 5 [1..]
-- [1,2,3,4,5]
-- >>> myTake 0 [1..]
-- []
-- >>> myTake 7 []
-- []

myCycle :: [a] -> [a]
myCycle (x:xs) = x : myCycle (xs ++ [x])

-- >>> take 5 $ myCycle [1..3]
-- [1,2,3,1,2]
-- >>> zip (myCycle [1..3]) ['a'..'h']
-- [(1,'a'),(2,'b'),(3,'c'),(1,'d'),(2,'e'),(3,'f'),(1,'g'),(2,'h')]

ackerman :: Int -> Int -> Int
ackerman 0 n = n+1
ackerman m 0 = ackerman (m-1) 1
ackerman m n = ackerman (m-1) (ackerman m (n-1))

-- >>> :set +s
-- >>> ackerman 3 3
-- 61
-- (0.00 secs, 441,224 bytes)
-- >>> ackerman 3 8
-- 2045
-- (0.96 secs, 482,792,536 bytes)


-- Q8.1

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- >>> myReverse [1..5]
-- [5,4,3,2,1]
-- (0.01 secs, 72,216 bytes)

