-- Lesson 7 -> rules for recursion and pattern matching

-- My version of 'take' before reading the lesson

my_take n l = my_take_aux n l []
  where
    my_take_aux n [] new_l = new_l
    my_take_aux 0 _ new_l = new_l
    my_take_aux n (x:xs) new_l = x : (my_take_aux (n-1) xs new_l)

-- >>> my_take 3 [1,2,3,4]
-- [1,2,3]

myGCD a b = if remainder == 0
            then b
            else myGCD b remainder
  where remainder = a `mod` b

-- >>> myGCD 20 16
-- 4 (ok !)

-- Quick check 7.2
-- For the myGCD function, it doesnt matter

sayAmount n = case n of
                1 -> "one"
                2 -> "two"
                n -> "a bunch"

sayAmount_pm 1 = "one"
sayAmount_pm 2 = "two"
sayAmount_pm 3 = "a bunch"

-- >>> sayAmount 8
-- "a bunch"
-- >>> sayAmount_pm 2
-- "two"

myHead (x:xs) = x
myHead [] = error "no head for empty list"

-- >>> myHead []
-- *** Exception: no head for empty list
-- CallStack (from HasCallStack):
--   error, called at /var/folders/qk/gt5lkdkn1vs4_x8z_th7tpz00000gn/T/danteS1VjiB.hs:40:13 in main:Main

-- >>> myHead [1..]
-- 1

-- Quick check 7.3

myTail [] = error "no tail for a empty list"
myTail (_:xs) = xs

-- >>> myTail []
-- *** Exception: no tail for a empty list
-- CallStack (from HasCallStack):
--   error, called at /var/folders/qk/gt5lkdkn1vs4_x8z_th7tpz00000gn/T/danteS1VjiB.hs:52:13 in main:Main

-- >>> myTail [8]
-- []


-- Q7.1

-- look quick check 7.3 above

-- Q7.2

myGCD_ 0 _ = 0
myGCD_ _ 0 = error "divison per 0"
myGCD_ a b = if remainder == 0
            then b
            else myGCD_ b remainder
  where
    remainder = a `mod` b

-- >>> myGCD_ 0 7
-- 0
-- >>> myGCD 7 0
-- *** Exception: divide by zero
-- >>> myGCD 20 16
-- 4

