{-|
  Real World Haskell Chaptor 3
-}

module Chap3
  ( BookInfo
  , BookInfoTuple
  , BookInfo2
  , hoge
  , huga
  , hige
  ) where

-- BookInfo: Type Constructor
-- Book: Data Constructor
-- Int String [String]: Type Elements
data BookInfo = Book Int String [String]
                deriving (Show)
--usage
hoge :: BookInfo
hoge = Book 0 "0" ["0"]
{-
*Main> :info Book
data BookInfo = Book Int String [String]
  	-- Defined at Book/Book.hs:4:17
-}


-- Using Tuple and Type Synoinm
-- likes `typedef` of Clang, C++
type BookInfoTuple = (Int, String, [String])
--usage
huga :: BookInfoTuple
huga = (0, "0", ["0"])
{-
*Main> :info BookInfoTuple
type BookInfoTuple = (Int, String, [String])
  	-- Defined at Book/Book.hs:13:1
-}


data BookInfo2 = Book2 Int String [String]
                deriving (Show)
-- usge
hige :: BookInfo2
hige = Book2 0 "0" ["0"]
{-
*Main> :info Book2
data BookInfo2 = Book2 Int String [String]
  	-- Defined at Book/Book.hs:20:18
-}


{-
struct book_info {
  int id;
  char *name;
  char **authors;
};
-}


data Bool1 = False | True

type CardHolder = String
type CardNumber = String
type Address = [String]
type CustomerID = Int
-- BillingInfo: Type Constructor
-- CreditCard: Data Constructor
-- CashOnDelivery: Data Constructor
-- Invoice: Data Constructor
data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving(Show)


data Cartesian2D = Cartesian2D Double Double
                   deriving (Eq, Show)
data Polar2D = Polar2D Double Double
               deriving (Eq, Show)
type LinearSpace2D = (Double, Double)


-- # Enum
data Roygbiz = Red
            | Orange
            | Yellow
            | Green
            | Blue
            | Infigo
            | Violet
              deriving (Eq, Show)
{-
enum roygbiz {
  red, orange, yellow, green, blue, indigo, violet
};
-}

-- # Union Type
{-
enum shape_type {shape_circle, shape_poly};
struct circle { struct vector center; float radius; };
struct poly { size_t num_verticles; struct vector *verticles; };
struct shape {
  enum shape_type_type;
  union {
    struct circle circle;
    struct poly poly;
  } shape;
}
-}
type Vector = (Double, Double)
data Shape = Cricle Vector Double
           | Poly [Vector]


-- # Pattern Match
sumList :: Num a => [a] -> a
sumList (x:xs) = x + sumList xs
sumList [] = 0
{-
*AlgebraDataType> :type (+)
(+) :: Num a => a -> a -> a
-}

first (a, b, c) = a
second (a, b, c) = b
third (a, b, c) = c

-- Book Accessor
bookId (Book id title authors) = id
bookTitle (Book id title authors) = title
bookAuthors (Book id title authors) = authors
{-
*AlgebraDataType> bookAuthors (Book 0 "0" ["0"])
["0"]
-}


-- Record Syntax
data Customer = Customer {
    customerID :: CustomerID
  , customerName :: String
  , customerAddress :: Address
  } deriving (Show)

he :: Customer
he = Customer {
    customerID = 22
  , customerAddress = ["a","b","c"]
  , customerName = "poo"
  }
heAdress :: Address
heAdress = customerAddress he

{-
(defclass Customer (a b c))
(.customerAddress (new Customer 22 "poo" ["a" "b" "c"]))
-}


-- Case expression
fromMaybe :: a -> (Maybe a) -> a
fromMaybe defaultValue wrapped =
  case wrapped of
    Nothing -> defaultValue
    Just value -> value
{-
*AlgebraDataType> fromMaybe 3 (Just 0)
0
*AlgebraDataType> fromMaybe 3 Nothing
3
-}

-- Guad Syntax
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)
nodesAreSame (Node a _ _) (Node b _ _)
  | a == b = Just a
nodesAreSame _ _ = Nothing

lend3 amount balance
    | amount <= 0            = Nothing
    | amount < reserve * 0.5 = Nothing
    | otherwise              = Just newBalance
  where reserve = 100
        newBalance = balance - amount

myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n - 1) (tail xs)
niceDrop n xs
       | n <= 0   = xs
niceDrop _ []     = []
niceDrop n (_:xs) = niceDrop (n - 1) xs
