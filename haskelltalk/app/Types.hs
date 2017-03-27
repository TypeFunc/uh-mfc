module Types where

{-data declarations are used to create new 'types'.  "Rectangle" is the name of the new type, whereas "Rect" is the name of the type constructor. Sometimes the same name is used for both, but they are distinct! -}
data Rectangle a = Rect a a --length & width. "a" is a numeric type parameter (Float, Double, etc).

area :: Num a => Rectangle a -> a -- "Rectangle" is used in the type signature,
area (Rect x y) = x*y -- but we destructure on the type constructor "Rect".
--Note that the x and y in Rect x y are of the same type but can have different values.

data Area a = Area a --Create a data type to distinguish Doubles which represent Area from regular Doubles
area' :: Num a => Rectangle a -> Area a
area' (Rect x y) = Area (x*y) -- Notice we return Area (x*y) instead of just x*y
--This seemingly trivial change allows us to distinguish between values that represent different things, ***and have the compiler enforce it***!!!

cost :: Num a => Area a -> a -> a
cost (Area x) cost_per_unit_area = x*cost_per_unit_area

cost1 = cost (area' (Rect 3 4)) 2
--cost2 = cost (area (Rect 3 4)) 2 --compile error! :)

--Type classes
data List a b = Nil | Cons a b (List a b)
instance Functor (List a) where
  fmap f Nil = Nil
  fmap f (Cons a b list) = Cons a (f b) (fmap f list)

data MyType a = MT a
instance Functor MyType where
  fmap f (MT a) = MT (f a)

data Pair a b = P a b
instance Functor (Pair a) where
  fmap f (P a b) = P a (f b)

---------------------------------------------------------

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday  

type PhoneNumber = String  
type Name = String  
type PhoneBook = [(Name,PhoneNumber)]
phoneBook :: PhoneBook
phoneBook =      
    [("betty","555-2938"), ("bonnie","452-2928"), ("patsy","493-2928"), ("lucille","205-2928"), ("wendy","939-8282"), ("penny","853-2492")]

--recursive data types
data List' a = EmptyList | Cons' a (List' a) deriving (Eq, Read, Show)
myList1 :: List' Double
myList1 = Cons' 1.1 (Cons' 2.2 (Cons' 3.3 EmptyList))

data Tree a = Node a (Tree a) (Tree a) | EmptyTree deriving (Eq, Read, Show)

treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = Node x EmptyTree EmptyTree
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right)

myTree :: Tree Integer --foldl can take different types (& build instead of reduce)
myTree = foldl (flip treeInsert) EmptyTree [1..4] --flip switches arguments
--Node 1 EmptyTree (Node 2 EmptyTree (Node 3 EmptyTree (Node 4 EmptyTree EmptyTree)))

data Rose a = N a [Rose a] deriving (Eq, Read, Show) --N means Node
myRose :: Rose Integer
myRose = N 1 [N 2 [], N 3 [], N 4 [N 5 []]]

--mutually recursive data types :D
data Red   a b = R a | RR (Black a b) (Black a b) | L deriving (Eq, Read, Show)
data Black a b = B b | BB (Red a b) (Red a b) | M deriving (Eq, Read, Show)
rb1, rb3:: Red Integer Double
rb2, rb4:: Black Integer Double
rb1 = RR (B 1.0) M
rb2 = BB (R 2) L
rb3 = RR (BB (R 1) (R 2)) (B 3.0)
rb4 = BB (R 1) (RR (B 2.0) (BB (R 3) (R 4)))
