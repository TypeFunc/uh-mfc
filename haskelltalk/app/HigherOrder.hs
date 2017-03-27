module HigherOrder where

import Basics (plus1, plus1', plus1'', plus2, plus2', plus2'')

{-Now we can pass the plus functions as an *argument* to the map function!
  Functions which take other functions as an argument (i.e. map, zip, fold, etc)
  are so-called "higher-order functions" -}
mapped1  = map plus1   [1,2,3,4,5]
mapped2  = map plus1'  [1,2,3,4,5]
mapped3  = map plus1'' [1,2,3,4,5]
mapped4  = map (+1)    [1,2,3,4,5] --partial evaluation again

zipped1 = zip [1,2,3] ["one", "two", "three"]
--[(1,"one"),(2,"two"),(3,"three")]
--zipWith is like mapping over two lists
zipped2 = zipWith (+) [1,2,3] [4,5,6] --[5,7,9]
-- parenthesis are used to partially apply an infix operator to turn it into a prefix function
zipped3 = zipWith (,) [1,2,3] ["one", "two", "three"]
--(,) :: a -> b -> (a, b) So the comma is a pairing *function*, not just a special syntax!
plusthrees = zipWith (.) [plus1, plus1', plus1''] [plus2, plus2', plus2'']
--(.) :: (b -> c) -> (a -> b) -> a -> c So the dot is a *function*, not just a special syntax!

--Just for fun, we can define our own function composition
comp :: (b -> c) -> (a -> b) -> a -> c
comp g f x = g (f x)
-- comp g f x = f (g x) -- note: This is a compilation error!
prefixcomp = comp plus1 plus2 3
infixcomp  = plus1 `comp` plus2 $ 3
plusthrees' = zipWith comp [plus1, plus1', plus1''] [plus2, plus2', plus2'']

reduced1 = foldl (+) 0 [1,2,3,4,5] -- (((((0 + 1) + 2) + 3) + 4) + 5)
reduced2 = foldr (+) 0 [1,2,3,4,5] -- (1 + (2 + (3 + (4 + (5 + 0))))
reduced3 = scanl (+) 0 [1,2,3,4,5] -- [0,1,3,6,10,15]
reduced4 = scanr (+) 0 [1,2,3,4,5] -- [15,14,12,9,5,0]
