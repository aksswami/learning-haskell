-- file: ch03/BookStore.hs

data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

type CustomerID = Int
type ReviewBody = String

data BookReview = BookReview BookInfo CustomerID ReviewBody
                  deriving (Show)

type BookRecord = (BookInfo, BookReview)

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)

bookId        (Book id title authors) = id
bookTitle     (Book id title authors) = title
bookAutors    (Book id title authors) = authors

nicerid       (Book id _     _      ) = id
nicerTItle    (Book _  title _      ) = title
nicerAutors   (Book _  _     authors) = authors

data Customer = Customer {
      customerID      :: CustomerID
    , customerName    :: String
    , customerAddress :: Address
    } deriving (Show)


customer1 = Customer 271828 "J. R. hacker"
                    ["255 Syntax ct",
                    "Milapolis, CA 2323",
                    "USA"]

customer2 = Customer {
              customerID = 272818
            , customerName = "J. D. Brown"
            , customerAddress = ["Yellow Street", "Blue Jack", "Black USA"]
}
