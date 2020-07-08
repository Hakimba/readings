-- Lesson 18 parametrized types
import qualified Data.Map as Map
import qualified Data.List as L
data Box a = Box a deriving Show

-- >>> :t Box 7
-- Box 7 :: Num a => Box a

-- >>> :t Box ["Hey"]
-- Box ["Hey"] :: Box [[Char]]

-- Quick check 18.1
-- >>> :t Box (Box 'a')
-- Box (Box 'a') :: Box (Box Char)

-- >>> :k Box
-- Box :: * -> *

data Triple a = Triple a a a deriving Show

type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 0.6 0.97 0.2

type FullName = Triple String

aPerson :: FullName
aPerson = Triple "Howard" "Phillips" "Lovecraft"

-- accessors

first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _) = x

third :: Triple a -> a
third (Triple _ _ x) = x

transform :: (a->a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

-- >>> transform (*3) aPoint
-- Triple 1.7999999999999998 2.91 0.6000000000000001

-- >>> transform reverse aPerson
-- Triple "drawoH" "spillihP" "tfarcevoL"

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,23]

organPairs :: [(Int,Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

-- >>> Map.lookup 7 organCatalog
-- Just Heart

-- Q18.1

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple a b c) = Triple (f a) (f b) (f c)

-- >>> tripleMap show (Triple 6 5 5)
-- Triple "6" "5" "5"

boxMap :: (a->b) -> Box a -> Box b
boxMap f (Box val) = Box $ f val

-- >>> boxMap show (Box 78.8)
-- Box "78.8"

instance Ord Organ where
  compare a b | a == b = EQ
  compare Heart _ = GT
  compare _ Heart = LT
  compare Brain _ = GT
  compare _ Brain = LT
  compare Spleen _ = GT
  compare _ Spleen = LT


-- >>> Heart > Kidney
-- True

organs_ = [Spleen,Kidney,Brain,Heart]

organCatalog_ :: Map.Map Organ Int
organCatalog_ = Map.fromList $ zip organs_ (take 4 ids)

-- >>> Map.toList organCatalog_
-- [(Kidney,7),(Spleen,2),(Brain,13),(Heart,14)]

-- >>> Map.lookup Kidney organCatalog_
-- Just 7
