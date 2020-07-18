{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Semigroup

-- Text is implemented as an array under the hood, so its much more memory-efficient
-- Text is fully strict, use Text.Lazy if you need lazy evaluation

-- When to use Text vs String ?
-- some members of the haskell commu argue that the Prelude should be thrown out of anything practical due to the heavy dependency on String.
-- in a nutshell, use String for learning purposes, and use Text for anything else.

firstWord :: String
firstWord = "pessimism"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

-- >>> firstWord
-- "pessimism"
-- >>> secondWord
-- "pessimism"
-- >>> thirdWord
-- "pessimism"

-- avoid converting back and forth between text and string, its not 0(1)
-- use OverloadedString in order to be able to define Text as literal string

aWord :: T.Text
aWord = "hey\nyo\nyoupi"

-- >>> T.lines aWord
-- ["hey","yo","youpi"]

aText :: T.Text
aText = "hey how are you ?"

-- >>> T.words aText
-- ["hey","how","are","you","?"]

aEquation :: T.Text
aEquation = "67*65+67"

factors :: T.Text
factors = "*"

-- >>> T.splitOn factors aEquation
-- ["67","65+67"]

combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some"," ","text"]

combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text"

-- >>> combinedTextMonoid
-- "some text"

-- >>> combinedTextSemigroup
-- "some text"

-- >>> combinedTextMonoid == combinedTextSemigroup
-- True

myTlines :: T.Text -> [T.Text]
myTlines = T.splitOn "\n"

myTunlines :: [T.Text] -> T.Text
myTunlines = T.intercalate "\n"

-- >>> aWord
-- "hey\nyo\nyoupi"

-- >>> myTlines aWord
-- ["hey","yo","youpi"]

-- >>> myTunlines $ myTlines aWord
-- "hey\nyo\nyoupi"

-- Devanagari handlers

dharma :: T.Text
dharma = "धर्म"

-- >>> dharma
-- "\2343\2352\2381\2350"

bgText :: T.Text
bgText = " श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्।स्वधर्मे निधनं श्रेयः परधर्मो"

highlight :: T.Text -> T.Text -> T.Text
highlight query fulltext = T.intercalate highlighted pieces where
  pieces = T.splitOn query fulltext
  highlighted = mconcat ["{",query,"}"]

-- >>> highlight dharma bgText
-- " \2358\2381\2352\2375\2351\2366\2344\2381\2360\2381\2357{\2343\2352\2381\2350}\2379 \2357\2367\2327\2369\2339\2307 \2346\2352{\2343\2352\2381\2350}\2366\2340\2381\2360\2381\2357\2344\2369\2359\2381\2336\2367\2340\2366\2340\2381\2404\2360\2381\2357{\2343\2352\2381\2350}\2375 \2344\2367\2343\2344\2306 \2358\2381\2352\2375\2351\2307 \2346\2352{\2343\2352\2381\2350}\2379"



main_ :: IO ()
main_ = do
  TIO.putStrLn (highlight dharma bgText)


-- Q23.1

helloPerson :: T.Text -> T.Text
helloPerson name = "Hello" <> " " <> name <> "!"

main__ :: IO ()
main__ = do
  TIO.putStrLn "Hello ! whats your name ?"
  name <- TIO.getLine
  let statement = helloPerson name
  TIO.putStrLn statement

-- Q23.2

toInts :: T.Text -> [Int]
toInts  = map (read . T.unpack) . T.lines

main = do
  userInput <- TIO.getContents
  let numbers = toInts userInput
  TIO.putStrLn ((T.pack . show . sum) numbers)
