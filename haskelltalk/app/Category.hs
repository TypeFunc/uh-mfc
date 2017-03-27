module Category where

{-  A category is a collection of "objects" and "morphisms" such that composition of morphisms is
associative and there is an identity morphism for each object.  I.E, given morphisms
f:A -> B, g:B -> C, and h:C -> D then (h . g) . f = h . (g . f) and given
idA: A -> A, idB:B -> B, etc idB . f = f . idA = f  So a category is simply an abstract notion of
composition.  Categories work almost the same in haskell as they do mathematically.

The collection of all haskell data types forms a category called Hask.
The object are types and the morphisms are type constructors, i.e.
Just :: a -> Maybe a
(,) :: a -> b -> (a,b)
Remember that since types are algebraic, given a type A and a type B in Hask,
the types A | B and (A,B) are also in Hask.

Categories are implemented using type classes.  Type classes are more like "interfaces"
(as opposed to the "classes" from object oriented programming).
-}
--TODO...
{- What the hell is a monad?  Easy, a monad is a monoid in the category of endofunctors
(of Hask). :)
-}
--TODO...
