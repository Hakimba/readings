doubleDouble x = (\dubs -> dubs*2) x*2

-- >>> doubleDouble 6
-- 24

-- a TERRIBLE implementation, we make the same severals computation for nothinga TERRIBLE implementation, we make the same computation several times for nothing
sumSquareOrSquareSum x y = if (x^2+y^2) > ((x+y)^2)
                           then	(x^2+x^2)
                           else	((x+y)^2)

-- >>> sumSquareOrSquareSum 3 2
-- 25

--A way better implementation, with 'where' clause
sumSquareOrSquareSum_where x y = if sumSquare > squareSum
                                 then sumSquare
                                 else squareSum	where
  sumSquare = (x^2+y^2)
  squareSum = ((x+y)^2)

-- >>> sumSquareOrSquareSum_where 3 2
-- 25

--A another way better implementation with lambda, without 'where' clause
--Javascript design pattern : IIFE (because javascript suck)
sumSquareOrSquareSum_lambda x y = (\sumSquare squareSum ->
                                     if sumSquare > squareSum
                                     then sumSquare
                                     else squareSum) (x^2+y^2) ((x+y)^2)
-- >>> sumSquareOrSquareSum_lambda 3 2
-- 25

--Equivalent to the where variant
sumSquareOrSquareSum_let x y = let sumSquare = (x^2+y^2)
                                   squareSum = ((x+y)^2)
                               in
                               	 if sumSquare > squareSum
				 then sumSquare
                                 else squareSum
-- >>> sumSquareOrSquareSum_let 3 2
-- 25

overwrite_lambda x = (\x -> (\x -> (\x -> x) 4 ) 3 ) x
-- >>> overwrite_lambda 2
-- 4

-- Lexical scope : search for a definition of a variable in the nearest scope. (javascript and haskell use that strategy)

--Impossible with 'let'
counter x = (\x -> (\x -> x+1) x+1) x
-- >>> counter 2
-- 4
