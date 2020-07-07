-- Lesson 17 Design by composition semigroups and monoids

import Data.List
import Data.Semigroup

myLast :: [a] -> a
myLast = head . reverse

myMin :: Ord a => [a] -> a
myMin = head . sort

myMax :: Ord a => [a] -> a
myMax = myLast . sort

myAll :: (a -> Bool) -> [a] -> Bool
myAll testFunc = (foldr (&&) True) . (map testFunc)

-- >>> myAll (>=2) [2..5]
-- True

-- Quick check 17.1

myAny :: (a -> Bool) -> [a] -> Bool
myAny testFunc = (foldr (||) False) . (map testFunc)

-- >>> myAny (>2) [1..3]
-- True
-- >>> myAny (>4) [1..4]
-- False

-- Quick check 17.2
-- We can't use (/) to make Int a Semigroup, because / works with Integer

data Color =
  Red |
  Yellow |
  Blue |
  Green |
  Purple |
  Orange |
  White  |
  Brown deriving (Show, Eq)

--instance Semigroup Color where
--  (<>) Red Blue = Purple
--  (<>) Blue Red = Purple
--  (<>) Yellow Blue = Green
--  (<>) Blue Yellow = Green
--  (<>) Yellow Red = Orange
--  (<>) Red Yellow = Orange
--  (<>) a b = if a == b then a else Brown

-- >>> Red <> Yellow
-- Orange

-- Many of typeclass have laws, associativity is one of the law of a semigroup, but the haskell compiler can't enforce these

instance Semigroup Color where
  (<>) Red Blue = Purple                                    
  (<>) Blue Red = Purple                                          
  (<>) Yellow Blue = Green                                        
  (<>) Blue Yellow = Green                                        
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
  (<>) a White = a
  (<>) White b = b
  (<>) a b | a == b = a
           | all (`elem` [Red,Blue,Purple]) [a,b] = Purple
           | all (`elem` [Blue,Yellow,Green]) [a,b] = Green
           | all (`elem` [Red,Yellow,Orange]) [a,b] = Orange
           | otherwise = Brown


-- >>> (Yellow <> Green) <> Blue
-- Green

associativity_law :: Color -> Color -> Color -> Bool
associativity_law a b c = (a <> b) <> c == a <> (b <> c)

-- >>> associativity_law Red Blue Purple
-- True

-- >>> associativity_law Red Orange Blue
-- True

-- Quick check 17.4
-- if we implement mappend / (<>) for Integer as *, the mempty value will be 1, because 1 is the neutral element of the multiplication.
-- 1 * n = n

-- Monoid
-- First law  : mappend mempty x = x (neutral element)
-- Second law : mappend x mempty = x (neutral element symetry)
-- Third law : associativity of mappend
-- Fourth law : mconcat = foldr mappend mempty

type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

createTable :: Events -> Probs -> PTable
createTable events probs = PTable events normalizedProbs where
  totalProbs = sum probs
  normalizedProbs = map (\x -> x / totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, " | ", show prob, "\n"]

instance Show PTable where
  show (PTable events probs) = mconcat pairs
    where pairs = zipWith showPair events probs

-- >>> createTable ["Pile","Face"] [0.5,0.5]
-- Pile | 0.5
-- Face | 0.5

cartesianProduct :: (a -> b -> c) -> [a] -> [b] -> [c]
cartesianProduct func l1 l2 = zipWith func newL1 cycledL2
  where nToAdd = length l2
        repeatedL1 = map (take nToAdd . repeat) l1
        newL1 = mconcat repeatedL1
        cycledL2 = cycle l2

-- >>> cartesianProduct (*) [1..3] [1..3]
-- [1,2,3,2,4,6,3,6,9]

combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartesianProduct combiner e1 e2 where
  combiner = (\x y -> mconcat [x,"-",y])

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartesianProduct (*) p1 p2

instance Semigroup PTable where
  (<>) ptable1 (PTable [] []) = ptable1
  (<>) (PTable [] []) ptable2 = ptable2
  (<>) (PTable e1 p1) (PTable e2 p2)  = createTable newEvents newProbs
    where newEvents = combineEvents e1 e2
          newProbs = combineProbs p1 p2

-- >>> let coin = createTable ["Pile","Face"] [0.5,0.5]
-- >>> coin
-- Pile | 0.5
-- Face | 0.5
-- >>> coin <> coin
-- Pile-Pile | 0.25
-- Pile-Face | 0.25
-- Face-Pile | 0.25
-- Face-Face | 0.25

instance Monoid PTable where
  mempty = PTable [] []
  mappend = (<>)

-- >>> let coin = createTable ["Pile","Face"] [0.5,0.5] 
-- >>> mconcat [coin,coin,coin]
-- Pile-Pile-Pile | 0.125
-- Pile-Pile-Face | 0.125
-- Pile-Face-Pile | 0.125
-- Pile-Face-Face | 0.125
-- Face-Pile-Pile | 0.125
-- Face-Pile-Face | 0.125
-- Face-Face-Pile | 0.125
-- Face-Face-Face | 0.125

-- Q17.1

instance Monoid Color where
  mempty = White
  mappend = (<>)


-- >>> mappend Yellow mempty
-- Yellow

-- Q17.2

