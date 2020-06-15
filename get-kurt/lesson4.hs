--Lesson 4 -> First class functions benefits

import Data.List as DL

ifEvenInc n = if even n then n+1 else n

-- >>> ifEvenInc 5
-- 5
-- >>> ifEvenInc 6
-- 7

ifEvenDouble n = if even n then n*2 else n

-- >>> ifEvenDouble 5-- <interactive>:68:2-11: error:
-- 5

ifEvenSquare n = if even n then n^2 else n

-- >>> ifEvenSquare 5
-- 5
-- >>> ifEvenSquare 6
-- 36

-- Here we can show the benefit of first class functions
-- our three functions have the same logic, only the computation we made if n is even change. So we can pass a function as argument

ifEvenDoF :: Int -> (Int -> Int) -> Int
ifEvenDoF n f = if even n then f n else n

-- >>> ifEvenDoF 5 (+1)
-- 5
-- >>> ifEvenDoF 6 (+1)
-- 7
-- >>> ifEvenDoF 6 (^2)
-- 36

ifEvenInc_ n = ifEvenDoF n (+1) -- or (\x -> x+1)
ifEvenDouble_ n = ifEvenDoF n (*2) -- or (\x -> x*2)
ifEvenSquare_ n = ifEvenDoF n (^2) -- or (\x -> x^2)

-- >>> ifEvenInc_ 6
-- 7
-- >>> ifEvenDouble_ 6
-- 12
-- >>> ifEvenSquare_ 6
-- 36

-- CAREFUL : Functions are always evaluated before operatos

-- >>> 1+2*3
-- 7 (mathematically ok)

inc n = (+1) n
-- >>> inc 2 * 3
-- 9 (mathematically not ok)

-- Quickcheck 4.1

ifEvenCubing n = ifEvenDoF n (^3) -- or (\x -> x^3)

-- >>> ifEvenCubing 2
-- 8
-- >>> ifEvenCubing 3
-- 3

type Person = (String, String) -- firstname, lastname

compareLastNames :: Person -> Person  -> Ordering
compareLastNames name1 name2 = if lastn1 > lastn2
                               then GT
                               else if lastn2 > lastn1
                                    then LT
                                    else EQ
  where
    lastn1 = snd name1
    lastn2 = snd name2

-- >>> let hakim = ("Hakim","Baaloudj") :: Person
-- >>> let david = ("David","Lafarge") :: Person

-- >>> compareLastNames hakim david
-- LT

-- >>> let manuel = ("Manu","Ferrara") :: Person
-- >>> let coworkers = [david,hakim,manuel]

-- >>> DL.sortBy compareLastNames coworkers
-- [("Hakim","Baaloudj"),("Manu","Ferrara"),("David","Lafarge")]



-- Quick check 4.2

compareLastNames_ :: Person -> Person -> Ordering
compareLastNames_ name1 name2 = if lastn1 > lastn2
                               then GT
                               else
                                 if lastn1 > lastn2
                                 then LT
                                 else compareFirstNames firstn1 firstn2
  where
    compareFirstNames f1 f2 = if f1 > f2 then GT
                              else if f1 < f2 then LT
                                   else EQ
    lastn1 = snd name1
    lastn2 = snd name2
    firstn1 = fst name1
    firstn2 = fst name2


-- >>> let hakim = ("Hakim","Baaloudj") :: Person
-- >>> let david = ("David","Lafarge") :: Person
-- >>> let manuel = ("Manu","Ferrara") :: Person
-- >>> let farid = ("Farid","Baaloudj") :: Person
-- >>> let coworkers = [david,hakim,manuel,farid]

-- >>> DL.sortBy compareLastNames_ coworkers-- <interactive>:49:30-38: error:
-- [("Farid","Baaloudj"),("Hakim","Baaloudj"),("Manu","Ferrara"),("David","Lafarge")]


-- Returning functions

type Adress = String

sfOffice :: Person -> Adress
sfOffice name = if lastName < "L"
                then nameText ++ " - PO Box 1234 - SF, CA, 94111"
                else nameText ++ " - PO Box 1234 - SF, CA, 94109"
  where
    lastName = snd name
    nameText = (fst name) ++ " " ++ lastName

-- >>> sfOffice (("hakim","baaloudj") :: Person)
-- "hakim baaloudj - PO Box 1234 - SF, CA, 94109"

nyOffice :: Person -> Adress
nyOffice name = nameText ++ ": PO Box 789 - NY, 10013" where
  nameText = (fst name) ++ " " ++ (snd name)

-- >>> nyOffice (("hakim","baaloudj") :: Person)
-- "hakim baaloudj: PO Box 789 - NY, 10013"

renoOffice :: Person -> Adress
renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where nameText = snd name

-- >>> renoOffice (("hakim","baaloudj") :: Person)
-- "baaloudj - PO Box 456 - Reno, NV 89523"

washOffice :: Person -> Adress
washOffice name = nameText ++ " PO Box 657 - DC 56087"
  where nameText = (fst name) ++ " " ++ (snd name) ++ " Esq"


getLocationFunction :: String -> (Person -> Adress)
getLocationFunction location = case location of
  "ny" -> nyOffice
  "sf" -> sfOffice
  "reno" -> renoOffice
  "wash" -> washOffice
  otherwise -> (\name -> (fst name ) ++ " " ++ (snd name))

adressLetter name location = getLocationFunction location $ name

-- >>> let hakim = ("hakim","baaloudj") :: Person

-- >>> getLocationFunction "ny" $ hakim
-- "hakim baaloudj: PO Box 789 - NY, 10013"

-- >>> getLocationFunction "france" $ hakim
-- "hakim baaloudj"

-- >>> adressLetter hakim "ny"
-- "hakim baaloudj: PO Box 789 - NY, 10013"

-- Q4.2
-- >>> adressLetter hakim "wash"
-- "hakim baaloudj Esq PO Box 657 - DC 56087"

-- Q4.1
compareLastNames__ :: Person -> Person -> Ordering
compareLastNames__ p1 p2 = case compare lastname1 lastname2 of
  EQ -> compare firstname1 firstname2
  response -> response
 where
   lastname1 = snd p1
   lastname2 = snd p2
   firstname1 = fst p1
   firstname2 = fst p2

-- >>> let hakim = ("Hakim","Baaloudj") :: Person
-- >>> let david = ("David","Lafarge") :: Person
-- >>> let farid = ("Farid","Baaloudj") :: Person

-- >>> compareLastNames__ hakim david
-- LT

-- >>> compareLastNames__ hakim farid
-- GT

-- >>> compareLastNames hakim hakim
-- EQ

-- Q4.2 (see above)

-- End of lesson
