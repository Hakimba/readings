-- Capstone : functional object oriented programming with robots !

-- A cup of coffee

cup fl0z = \message -> message fl0z

-- >>> aCup = cup 6

getOz acup = acup (\fl0z -> fl0z)

-- >>> aCup = cup 6
-- >>> getOz aCup
-- 6

drink acup ozDrank = cup (fl0z - ozDrank)
  where
    fl0z = getOz acup

-- >>> aCup  = cup 20
-- >>> getOz $ drink aCup 10
-- 10

-- Robots


