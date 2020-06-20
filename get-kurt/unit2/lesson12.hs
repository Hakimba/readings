-- Lesson 12 -> Creating your own types

-- Using type synonyms

patientInfo :: String -> String -> Int -> Int -> String
patientInfo fname lname age height = name ++ " " ++ageHeight
  where
    name = fname ++ ", " ++ lname
    ageHeight = "(" ++ show age ++ "yrs. "++show height++ "in.)"

type Firstname = String
type Lastname = String
type Age = Int
type Height = Int

-- patientInfo :: Firstname -> Lastname -> Age -> Height

type PatientName = (String,String)

firstname :: PatientName -> Firstname
firstname (fstn,_) = fstn

lastname :: PatientName -> Lastname
lastname (_,sndn) = sndn

-- >>> firstname $ (("Hakim","Baaloudj") :: PatientName)
-- "Hakim"

patientInfo_ :: PatientName -> Age -> Height -> String
patientInfo_ pt age height = name ++ " " ++ageHeight
  where
    name = (fst pt) ++ ", " ++ (snd pt)
    ageHeight = "(" ++ show age ++ "yrs. "++show height++ "in.)"

data Sex = Male | Female

-- Sex -> type constructor
-- Male | Female -> data constructor (constructor of instance of Sex)

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType

patient0BT :: BloodType
patient0BT = BloodType A Pos

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo (BloodType O _) _ = True
canDonateTo _ _ = False

-- Records

data Patient = Patient {name :: PatientName
                       , sex :: Sex
                       , age :: Int
                       , height :: Int
                       , weight :: Int
                       , bloodType :: BloodType }

-- Q12.1
canDonateTo_ :: Patient -> Patient -> Bool
canDonateTo_ p1 p2 = canDonateTo (bloodType p1) (bloodType p2)

-- Q12.2
-- c'est du print.. flemme.
