-- Lesson 14 -> Using type classes

data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 -- deriving (Show)
-- >>> S1
-- S1
-- >>> S6
-- S6

instance Show SixSidedDie where
  show S1 = "one"
  show S2 = "two"
  show S3 = "three"
  show S4 = "four"
  show S5 = "five"
  show S6 = "six"

-- >>> S1
-- one
-- >>> S4
-- four

instance Eq SixSidedDie where
  (==) S1 S1 = True
  (==) S2 S2 = True
  (==) S3 S3 = True
  (==) S4 S4 = True
  (==) S5 S5 = True
  (==) S6 S6 = True
  (==) _ _   = False

-- (/=) is automatically build, because /= is just : not (==)
-- >>> S1 /= S2
-- True

-- Quick check 14.2
-- The minimal definition of the RealFrac typeclass is 
-- properFraction :: Integral b => a -> (b, a)

instance Ord SixSidedDie where
  compare S6 S6 = EQ
  compare S6 _ = GT
  compare _ S6 = LT
  compare S5 S5 = EQ
  compare S5 _ = GT
  compare _ S5 = LT
  compare S4 S4 = EQ
  compare S4 _ = GT
  compare _ S4 = LT
  compare S3 S3 = EQ
  compare S3 _ = GT
  compare _ S3 = LT
  compare S2 S2 = EQ
  compare S2 _ = GT
  compare _ S2 = LT
  compare S1 S1 = EQ

-- >>> S1 > S2
-- False
-- >>> S3 >= S1
-- True
-- >>> S1 <= S5
-- True

-- What about use deriving Ord directly ?

data Test1 = AA | ZZ deriving (Eq,Ord)

-- >>> AA >= ZZ
-- False
-- >>> ZZ > AA
-- True

-- default behavior is to give a order that the data constructors are defined

data Days = Lundi | Mardi | Mercredi | Jeudi | Vendredi | Samedi | Dimanche deriving (Show, Eq, Ord, Enum)

-- une semaine
-- >>> [Lundi .. ]
-- [Lundi,Mardi,Mercredi,Jeudi,Vendredi,Samedi,Dimanche]

-- le sport tout les deux jours
-- >>> [Lundi,Mercredi ..]
-- [Lundi,Mercredi,Vendredi,Dimanche]

-- >>> let lendemain_de = succ
-- >>> lendemain_de Lundi
-- Mardi

-- >>> let veille_de = pred
-- >>> veille_de Dimanche
-- Samedi

-- Lundi c'est avant mardi
-- >>> Lundi < Mardi
-- True

-- In lesson 4 we defined a function in order to pass it to sortBy (and defined a order), but we can directly defined a order by making my type, instance of Ord, and implement compare with the right order we want.

-- newtype is better when why have only one constructor
newtype  Name = Name (String,String) deriving (Show, Eq)
instance Ord Name where
  compare (Name (f1,l1)) (Name (f2,l2)) = compare (l1,f1) (l2,f2)

-- >>> import Data.List
-- >>> Name ("hey","yo")
-- Name ("hey","yo")
-- >>> let names = [Name ("Akim","Baal"), Name ("Basile","Amer")]
-- >>> sort names
-- [Name ("Basile","Amer"),Name ("Akim","Baal")]


-- Q14.1
data Test = B1 | B2 | B3 deriving (Show,Enum)

instance Eq Test where
  (==) v1 v2 = fromEnum v1 == fromEnum v1

instance Ord Test where
  compare v1 v2 = compare (fromEnum v1) (fromEnum v2)

-- >>> B1 == B1
-- True
-- >>> B1 <= B2
-- True
-- >>> B2 < B1
-- False

-- Q14.2

data FiveSideDie = D1 | D2 | D3 | D4 | D5 deriving (Show,Eq,Ord)

class Ord a => Die a where
  greatestFaceOf :: a -- greatest .....
  smallestFaceOf :: a -- smallest face of the die

instance Die FiveSideDie where
  greatestFaceOf = D5
  smallestFaceOf = D1

-- >>> greatestFaceOf :: FiveSideDie
-- D5
-- >>> smallestFaceOf :: FiveSideDie
-- D1
