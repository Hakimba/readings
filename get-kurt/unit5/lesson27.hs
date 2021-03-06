-- Lesson 27 : functor

-- quick check 27.1
import qualified Data.Map as Map

reverseMaybe :: Maybe String -> Maybe String
reverseMaybe (Just txt) = Just $ reverse txt
reverseMaybe Nothing = Nothing

-- >>> reverseMaybe $ Just "test"
-- Just "tset"

--instance Functor Maybe where
--  fmap func (Just n) = Just (func n)
--  fmap func Nothing = Nothing

-- quick check 27.2
-- >>> reverse <$> Just "test"
-- Just "tset"

-- >>> reverse <$> Nothing
-- Nothing

data RobotPart = RobotPart{
  name :: String
  , description :: String
  , cost :: Double
  , count :: Int
  } deriving Show

leftArm :: RobotPart
leftArm = RobotPart {
  name = "left arm"
  , description = "left arm for face punching"
  , cost = 1000.0
  , count = 3
  }

rightArm :: RobotPart
rightArm = RobotPart {
  name = "right arm"
  , description	= "right arm for kind ahdn gestures"
  , cost = 1025.0
  , count = 5
  }

robotHead :: RobotPart
robotHead = RobotPart {
  name = "robot head"
  , description	= "this head looks big"
  , cost = 5977.7
  , count = 2
  }

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part = mconcat ["<h2>",partName,"</h2>",
                          "<p><h3>desc</h3>",partDesc,
                          "</p><p><h3>cost</h3>",
                          partCost,
                          "</p><p><h3>count</h3>",
                          partCount,"</p>"]
  where partName = name part
        partDesc = description part
        partCost = show $ cost part
        partCount = show $ count part

-- >>> renderHtml leftArm
-- "<h2>left arm</h2><p><h3>desc</h3>left arm for face punching</p><p><h3>cost</h3>1000.0</p><p><h3>count</h3>3</p>"

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where keys = [1..3]
        vals = [leftArm,rightArm,robotHead]
        keyVals = zip keys vals

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

-- >>> partVal
-- Just (RobotPart {name = "left arm", description = "left arm for face punching", cost = 1000.0, count = 3})

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

allParts :: [RobotPart]
allParts = map snd (Map.toList partsDB)

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

-- Q27.1

data Box a = Box a deriving Show

instance Functor Box where
  fmap f (Box a) = Box (f a)

morePresents :: Int ->  Box a -> Box [a]
morePresents n box = fmap (\a -> take n $ repeat a) box

-- >>> morePresents 5 (Box 4)
-- Box [4,4,4,4,4]
