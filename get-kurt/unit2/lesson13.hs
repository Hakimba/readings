-- Lesson 13 -> Typeclass

class Describable a where
  describe :: a -> String

-- Quick check 13.3

data Flavor = Chocolate | Vanilla | Strawberry deriving (Eq,Ord)

-- >>> Chocolate > Vanilla
-- False
-- >>> Strawberry > Chocolate
-- True
-- >>> Strawberry > Vanilla
-- True

-- Q13.1
-- >>> :info Word
-- data Word = GHC.Types.W# GHC.Prim.Word#
-- Defined in ‘GHC.Types’
-- instance Eq Word -- Defined in ‘GHC.Classes’
-- instance Ord Word -- Defined in ‘GHC.Classes’
-- instance Enum Word -- Defined in ‘GHC.Enum’
-- instance Num Word -- Defined in ‘GHC.Num’
-- instance Real Word -- Defined in ‘GHC.Real’
-- instance Show Word -- Defined in ‘GHC.Show’
-- instance Read Word -- Defined in ‘GHC.Read’
-- instance Bounded Word -- Defined in ‘GHC.Enum’
-- instance Integral Word -- Defined in ‘GHC.Real’

-- Q13.2
-- >>> :info Enum
-- class Enum a where
--   succ :: a -> a
--   pred :: a -> a
--   toEnum :: Int -> a
--   fromEnum :: a -> Int
--   enumFrom :: a -> [a]
--   enumFromThen :: a -> a -> [a]
--   enumFromTo :: a -> a -> [a]
--   enumFromThenTo :: a -> a -> a -> [a]

--   {-# MINIMAL toEnum, fromEnum #-}
--   	-- Defined in ‘GHC.Enum’

-- instance Enum Word -- Defined in ‘GHC.Enum’
-- instance Enum Ordering -- Defined in ‘GHC.Enum’
-- instance Enum Integer -- Defined in ‘GHC.Enum’
-- instance Enum Int -- Defined in ‘GHC.Enum’
-- instance Enum Char -- Defined in ‘GHC.Enum’
-- instance Enum Bool -- Defined in ‘GHC.Enum’
-- instance Enum () -- Defined in ‘GHC.Enum’
-- instance Enum Float -- Defined in ‘GHC.Float’
-- instance Enum Double -- Defined in ‘GHC.Float’

-- inc is limited to the type Int, Enum have instance for Int and Integer so succ works on those types, and Int must be a Enum instance.
