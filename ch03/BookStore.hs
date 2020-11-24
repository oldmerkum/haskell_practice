-- file: ch03/BookStore.hs
data BookInfo = Book Int String [String]
    deriving (Show)

data MagazineInfo = Magazine Int String [String]
    deriving (Show)

myInfo = Book 456 "A book" ["me", "you"]

type CustomerID = Int
type ReviewBody = String
type Address = [String]

data BookReview = BookReview BookInfo CustomerID ReviewBody

type BookRecord = (BookInfo, BookReview)

bookID (Book id title authors) = id
bookID (Book id title authors) = title
bookID (Book id title authors) = authors

data Customer = Customer {
    customerID :: CustomerID
    , customerName :: String
    , customerAddress :: Address
} deriving (Show)
