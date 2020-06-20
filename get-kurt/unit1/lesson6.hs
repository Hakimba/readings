-- Lesson 6 -> Lists

-- >>> zip (cycle [1..3]) [1..6]
-- [(1,1),(2,2),(3,3),(1,4),(2,5),(3,6)]

repeat_ n = cycle [n]

-- take 5 $ repeat_ 'A'

-- Q6.2

subseq start end l = drop start $ take end l
-- >>> subseq 2 5 [1 .. 10]
-- [3,4,5]
-- >>> subseq 2 7 "a puppy"
-- "puppy"

-- Q6.3

inFirstHalf l el = elem el half_l
  where
    half_l = take ((length l) `div` 2) l

-- >>> inFirstHalf [1..5] 2
-- True
-- >>> inFirstHalf ['a'..'z'] 's'
-- False
