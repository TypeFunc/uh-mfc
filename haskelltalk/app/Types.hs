module Types where

import Prelude hiding (Maybe, Either)

{-  In C/C++/Java, etc. an Integer is just a certain pattern of bits in memory.  In Haskell, types are more algebraic. Think of types as "assertions as to the potential values of a given term" not "storage classification systems".  data declarations are used to create new 'types'.  "Rectangle" is the name of the new type, whereas "Rect" is the name of the type constructor. Sometimes the same name is used for both, but they are distinct! -}
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

------------------Some Common Types----------------------------
data Maybe a = Nothing | Just a
{- Maybe is used for computations which may fail.  Instead of throwing an exception or returning
a null pointer, we return the Nothing data constructor, and then the compiler can enforce that
the caller handles both constructors. This is a Big Deal.
The vertical bar | is used to form the sum (basically disjoint union) of two types. -}
data  Either a b  =  Left a | Right b
{- Either is also used for computations which may fail.  Instead of returning Nothing,
we can return some useful information (such as an error message). Again, the compiler
can enforce that the caller handles both constructors. -}
---------------------------------------------------------------

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

-- TODO insert

{-Type classes are like "interfaces" (as opposed to the "classes" from object oriented programming)
, i.e. any type which is an instance of a type class must implement all of the class functions.
Suppose we didn't like the way lists and pairs are printed by default.  We can do this:  -}
class StringLike a where
  toString :: a -> String

instance Show a => StringLike [a] where
  toString xs = concat $ map show xs
instance (Show a, Show b) => StringLike (a,b) where
  toString (x,y) = show x ++ show y
instance Show a => StringLike (Area a) where
  toString (Area a) = "Area: " ++ show a

makeString :: StringLike a => a -> String
makeString x = toString x
-- The point of type classes is that we can write code that works for any type in the class:
liststring = makeString [1,2,3]    -- "123" 
pairstring = makeString ('x','y')  -- "'x''y'"
areastring = makeString $ Area 51  -- "Area: 51"
-- Type classes allow us to bundle together separate types, which is very useful.
