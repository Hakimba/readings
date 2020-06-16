-- Lesson 5 -> Closures and partial application

ifEven f x = if even x then f x else x

ifEvenInc n = ifEven (+1) n
ifEvenDouble n = ifEven (*2) n
ifEvenSquare n = ifEven (^2) n


-- Here, we captured a function 'f' into a lambda
-- We call that a 'closure'
genIfEven f = (\x -> ifEven f x)

ifEvenInc_ = genIfEven (+1)

-- >>> genIfEven (*2) $ 5
-- 5
-- >>> genIfEven (^2) $ 6
-- 36
-- >>> ifEvenInc_ 6
-- 7

-- Quick check 5.1

genIfXEven x = (\f -> ifEven f x)

-- >>> genIfXEven 6 $ (+1)
-- 7

getRequestURL host apiKey resource id = host ++
                                        "/" ++
                                        resource ++
                                        "/" ++
                                        id ++
                                        "?token="++
                                        apiKey
-- >>> getRequestURL "http://example.com" "1337hAsk3ll" "book" "1224"
-- "http://example.com/book/1224?token=1337hAsk3ll"

-- Anytime you might want to use a closure, you want to order
-- your arguments from most to least general

-- Its tedious to specify host and apikey everytime we want to make a query
-- So, closure are useful for that case, we capture host and apikey only once
-- because there are the most general argument, and we return a lambda
-- which wait for the leastest arguments (resource, id, which potentially change at every query)

genHostRequestBuilder host apikey = (\resource id -> getRequestURL host apikey resource id)

-- >>> let explQuery = genHostRequestBuilder "http://example.com" "123AkfR"

-- >>> explQuery "films" "123"
-- "http://example.com/films/123?token=123AkfR"

-- >>> explQuery "books" "197"
-- "http://example.com/books/197?token=123AkfR"


-- Quick check 5.2
genApiRequestBuilder host apikey resource = (\id -> getRequestURL host apikey resource id)

-- >>> let exampleApiQuery = genApiRequestBuilder "http://example.com" "123AkfR"
-- >>> let booksOfExmpApiQuery = exampleApiQuery "books"

-- >>> booksOfExmpApiQuery "123"
-- "http://example.com/books/123?token=123AkfR"


-- Ok, closure are cool and powerful, and useful, but what about partial application ? (curryfication)
-- partial application do the same as closures, natively in haskell

-- >>> let add3To = (+) 3
-- >>> add3To 5
-- 8


-- Quick check 5.3

-- >>> let exampleApiQuery = genApiRequestBuilder "http://example.com" "123AkfR"
-- >>> let booksOfExmpApiQuery = exampleApiQuery "books"

-- >>> booksOfExmpApiQuery "123"                                               
-- "http://example.com/books/123?token=123AkfR"


-- A good practice is to order arguments from most to least general in order to  make easier use of partial application

-- Quick check 5.4
subtrac2 = flip (-) 2

-- >>> subtrac2 5
-- 3

-- Q5.1
ifEven_ f x = if even x then f x else x

ifEvenInc__ = ifEven (+1)
ifEvenDouble__ = ifEven (*2)
ifEvenSquare__ = ifEven (^2)

-- Q5.2

binaryPartialApplication binF x = (\y -> binF x y)

-- >>> let plus2 =  binaryPartialApplication (+) 2-- <interactive>:1262:2-31: error:
-- >>> plus2 5
-- 7

-- End of lesson
