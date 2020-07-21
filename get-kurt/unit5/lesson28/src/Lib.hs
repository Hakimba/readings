module Lib where                              
-- Lesson 28 : Applicative type class                                       

-- >>> (++) <$> Just "Hakim " <*> Just "Baaloudj"                           
-- Just "Hakim Baaloudj"                                                    

import qualified Data.Map as Map

type LatLong = (Double, Double)

locationDB :: Map.Map String LatLong
locationDB = Map.fromList [("Arkham",(42.876,-70.876))
                           ,("Innsmouth",(42.785,-70.976))
                           ,("Carcosa",(29.874,-90.7856))
                           ,("New york",(40.7765,-73.876))]

-- >>> locationDB
-- fromList [("Arkham",(42.876,-70.876)),("Carcosa",(29.874,-90.7856)),("Innsmouth",(42.785,-70.976)),("New york",(40.7765,-73.876))]

toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double,Double)
latLongToRads (lat,long) = (rlat,rlong)
  where rlat = toRadians lat
        rlong = toRadians long

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
  where (rlat1,rlong1) = latLongToRads coords1
        (rlat2,rlong2) = latLongToRads coords2
        dlat = rlat2 - rlat1
        dlong = rlong2 - rlong1
        a = (sin (dlat/2))^2 + cos rlat1 * cos rlat2 * (sin (dlong/2))^2
        c = 2 * atan2 (sqrt a) (sqrt (1-a))
        earthRadius = 3961.0

-- >>> haversine (40.7776,-73.9691) (42.6054,-70.7829)                      
-- 207.3909006336738

printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "error, invalid city entered"
printDistance (Just dist) = putStrLn $ show dist <> " miles"

-- we want to deal with missing city the user can ask for
-- but haversine must have this signature : Maybe LatLong -> Maybe... Maybe Double
-- naive solution

haversineMaybe :: Maybe LatLong -> Maybe LatLong -> Maybe Double
haversineMaybe Nothing _ = Nothing
haversineMaybe _ Nothing = Nothing
haversineMaybe (Just l1) (Just l2) = Just $ haversine l1 l2

-- poor solution for two reasons
-- 1 : you have to write wrapers for any similar function, its repetitive
-- 2 : you have to write a different version of hiversineMaybe for other similar context types such as IO

-- Try to solve this with functor

-- quick check 28.1
addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe Nothing _ = Nothing
addMaybe _ Nothing = Nothing
addMaybe (Just x) (Just y) = Just $ x+y

-- >>> addMaybe (Just 6) Nothing
-- Nothing
-- >>> addMaybe (Just 5) (Just 8)
-- Just 13

-- we want a chunk which have a signature like :
-- Functor f => (a -> b -> c) -> f a -> f b -> f c

-- quick check 28.2

-- distanceFromNY :: LatLong -> Double
-- distanceFromNY = haversine Ny

-- we want to generalize functor's fmap to work with multiple arguments
-- partial application ?

maybeIncrement :: Maybe (Integer -> Integer)
maybeIncrement = (+) <$> Just 1

-- The solution : Applicative type class

-- >>> maybeIncrement <*> Just 6
-- Just 7

-- >>> maybeIncrement <*> Nothing
-- Nothing

-- >>> (++) <$> Just "cats" <*> Just " and dogs"
-- Just "cats and dogs"

-- >>> (++) <$> Nothing <*> Just "hey"
-- Nothing

-- quick check 28.3

-- >>> let val1 = Just 10
-- >>> let val2 = Just 5
-- >>> (*) <$> val1 <*> val2
-- Just 50
-- >>> div <$> val1 <*> val2
-- Just 2
-- >>> mod <$> val1 <*> val2
-- Just 0


-- >>>  startingCity = Map.lookup "Carcosa" locationDB
-- >>> destCity = Map.lookup "Innsmouth" locationDB
-- >>> haversine <$> startingCity <*> destCity
-- Just 1412.8299553452764


someFunc :: IO ()
someFunc = do
  putStrLn "Enter the starting city name:"
  startingInput <- getLine
  let startingCity = Map.lookup startingInput locationDB
  putStrLn "Enter the destination city name:"
  destInput <- getLine
  let destCity = Map.lookup destInput locationDB
  let distance = haversine <$> startingCity <*> destCity
  printDistance distance


data User = User{
  name :: String
  , gamerId :: Int
  , score :: Int
  } deriving Show

serverUsername :: Maybe String
serverUsername = Just "Sue"

serverGamerid :: Maybe Int
serverGamerid = Just 1337

serverScore :: Maybe Int
serverScore = Just 9001

-- >>> User <$> serverUsername <*> serverGamerid <*> serverScore
-- Just (User {name = "Sue", gamerId = 1337, score = 9001})

-- >>> User <$> serverUsername <*> Nothing <*> serverScore
-- Nothing

