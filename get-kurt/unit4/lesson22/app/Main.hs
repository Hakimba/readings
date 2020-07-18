module Main where

import Lib
import System.Environment
import Control.Monad
import Data.List.Split
import Data.List

-- Quick chech 22.1
--main :: IO ()
--main = do
--  let t = [1..3]
--  datas <- mapM (\_ -> getLine) t
--  mapM_ putStrLn datas


main_ :: IO ()
main_ = do
  args <- getArgs
  let linesToRead = if length args > 0
                       then read (head args)
                    else 0 :: Int
  numbers <- myreplicateM linesToRead getLine
  let convert_numb = map read numbers :: [Int]
  print $ sum convert_numb

-- quick check 22.2

myreplicateM :: Monad m => Int -> m a -> m [a]
myreplicateM nb io_action = mapM (\_ -> io_action) [1..nb]

myreplicateM_ :: Monad m => Int -> m () -> m ()
myreplicateM_ nb io_action = mapM_ (\_ -> io_action) [1..nb]


-- Interacting with lazy I/O
-- sum_lazy part

toInts :: String -> [Int]
toInts = map read . lines

--do the same thing what main_ doing
main__ :: IO ()
main__ = do
  userInput <- getContents
  let numbers = toInts userInput
  print $ sum $ map (^2) numbers


--Q22.1

evalAdd :: [String] -> Int
evalAdd eq = sum $ map evalEquation eq
evalMul :: [String] -> Int
evalMul eq = foldl (*) 1 (map evalEquation eq)

evalEquation :: String -> Int
evalEquation equ = if (length isAdd) > 1
                     then evalAdd isAdd
                   else
                     if (length isMul) > 1
                     then evalMul isMul
                     else (read equ :: Int) where
  isAdd = splitOn "+" equ
  isMul = splitOn "*" equ
  

--main :: IO ()
--main = do
--  equation <- getLine
--  print $ evalEquation equation
 
