-- Lesson 16 creating types with "or" and "and"

type FirstName = String
type LastName = String
type MiddleName = String

data Name =
  Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  | TwoInitWithLast Char Char LastName

data Author = Author Name
data Artist = Person Name | Band String

data Creator = AuthorCreator Author | ArtistCreator Artist

hpLovecraft :: Creator
hpLovecraft = AuthorCreator $ Author $ TwoInitWithLast 'H' 'P' "Lovecraft"


data Book = Book {
  author :: Creator
  , isbn :: String
  , bookTitle :: String
  , bookYear :: Int
  , bookPrice :: Double
                 }

data VinylRecord = VinylRecord {
  artist :: Creator
  ,recordTitle :: String
  ,recordYear :: Int
  ,recordPrice :: Double
  }

data StoreItem = BookItem Book | RecordItem VinylRecord


-- haskell conflit about field name on record
price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record

madeBy :: StoreItem -> String
madeBy (BookItem book) = (bookTitle book) + "was made by : " + (show (author book))
madeBy (RecordItem record) = (recordTitle record) + "was made by : " + (show (artist record))

