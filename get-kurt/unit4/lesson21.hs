-- Lesson 21 : Hello world ! introducing IO types

helloPerson :: String -> String
helloPerson name = "Hello" <> " " <> name <> "!"

main :: IO ()
main = do
  putStrLn "hello ! whats your name ? "
  name <- getLine
  let statement = helloPerson name
  putStrLn statement

-- we parametrized the IO context with a empty tuple which represented "nothing" semantically
-- because this main return nothing, the last process is a print.
-- main break functions rules, is not a function, but a IO action

minDie :: Int
minDie = 1

maxDie :: Int
maxDie = 6

--main :: IO ()
--main = do
--  dieRoll <- randomRIO (minDie,maxDie)
--  putStrLn (show dieRoll)

