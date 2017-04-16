module Category where

-- Hide everything from the prelude except what we need
import Prelude (Int, Double, IO, Maybe (..), Either (..), (.), (+), ($), (<), (++), sqrt, getLine, putStrLn, foldr, concat, map, const)

{- A category is a collection of "objects" and "morphisms" such that composition of morphisms is
associative and there is an identity morphism for each object.  I.E, given morphisms
f:A -> B, g:B -> C, and h:C -> D then (h . g) . f = h . (g . f) and given
idA: A -> A, idB:B -> B, etc idB . f = f . idA = f  So a category is simply an abstract notion of
composition.  Categories work almost the same in haskell as they do mathematically.

With some caveats, the collection of all haskell data types and functions forms a category called Hask (https://wiki.haskell.org/Hask).
The objects are types and the morphisms are functions, i.e.
Just :: a -> Maybe a
(,) :: a -> b -> (a,b)
Remember that since types are algebraic, given a type A and a type B in Hask,
the types A | B and (A,B) are also in Hask.  Note that functions can be objects as well as morphisms.

Categories are implemented using type classes. The four most important
type classes in haskell are Functor, Monoid, Applicative, and Monad.  Foldable and Traversable are also very important.  For the rest, see https://wiki.haskell.org/Typeclassopedia

First note that in category theory, a functor is a map from objects to objects AND a map from morphisms to morphisms (with certain conditions), where both maps are typically denoted by the same letter F.  In haskell, these two maps are separate, but we can bundle them together using a type class. -}

class Functor f where              --f is typically a type constructor
  fmap :: (a -> b) ->  f a -> f b  --This is equivalent to
--fmap :: (a -> b) -> (f a -> f b) --i.e. the purpose of fmap is to lift f

{- Functors should obey the following laws (note: NOT checked by the compiler)
Law 1: fmap id = id                         -- i.e. fmap id_A = id_f(A)
Law 2: fmap (g . h) = (fmap g) . (fmap h)

So Functors are just a pair of functions where we pinky swear to make the diagram commute. -}
instance Functor [] where
  fmap = map
instance  Functor Maybe  where
  fmap _ Nothing  = Nothing
  fmap f (Just a) = Just (f a)
instance Functor (Either a) where
  fmap _ (Left x) = Left x
  fmap f (Right y) = Right (f y)
instance Functor ((->) r) where
  fmap = (.)
instance Functor ((,) a) where
  fmap f (x,y) = (x, f y)
-----------------------------------------Monoid----------------------------------------------------
class Monoid a where
  mempty  :: a
  mappend :: a -> a -> a
{- Monoids should obey the following laws (note: NOT checked by the compiler)
Law 1: mempty `mappend` x = x
Law 2: x `mappend` mempty = x
Law 3: (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z) -}
instance Monoid [a] where
  mempty  = []
  mappend = (++)
instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing
  Nothing `mappend` m = m
  m `mappend` Nothing = m
  Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
-- Note: There is no Monoid instance for Either because the instance wouldn't be unique:
-- We would have to choose whether to mappend over the Left or Right constructors.
instance Monoid b => Monoid (a -> b) where
  mempty _ = mempty
  mappend f g x = f x `mappend` g x
-----------------------------------------Applicative-----------------------------------------------
class Functor f => Applicative f where
  pure  :: a -> f a
  infixl 4 <*>
  (<*>) :: f (a -> b) -> f a -> f b
{- Applicatives should obey the following laws (note: NOT checked by the compiler)
Law 1: pure id <*> v = v
Law 2: pure f <*> pure x = pure (f x)
Law 3: u <*> pure y = pure ($ y) <*> u
Law 4: u <*> (v <*> w) = pure (.) <*> u <*> v <*> w
Law 5: fmap g x = pure g <*> x  -}
instance Applicative [] where
  pure x    = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]
instance Applicative Maybe where
    pure = Just
    Just f  <*> m  = fmap f m
    Nothing <*> _m = Nothing
instance Applicative (Either e) where
    pure          = Right
    Left  e <*> _ = Left e
    Right f <*> r = fmap f r
instance Applicative ((->) a) where
    pure = const
    (<*>) f g x = f x (g x)
instance Monoid e => Applicative ((,) e) where
  pure x = (mempty, x)
  (u, f) <*> (v, x) = (u `mappend` v, f x)
-----------------------------------------Monad-----------------------------------------------------
{- What the hell is a monad?  Easy, a monad is a monoid in the category of endofunctors (of Hask).
https://s3.amazonaws.com/kukuruku-co/uploads/images/00/00/01/2014/05/29/41b8afd406.png :)
In other words, a monad is the following type class: -}
class Applicative m => Monad m where
  return :: a -> m a --Note: return is NOT like the return in Java, etc. Just a naming coincidence.
  (>>=)  :: m a -> (a -> m b) -> m b -- Note: >>= is pronounced "bind"

{- 'm' is a type constructor (e.g., Maybe, Either, etc.) that implements the Monad typeclass
Monads should obey the following laws (note: NOT checked by the compiler)
Law 1: return x >>= f = f x
Law 2: m >>= return = m
Law 3: (m >>= f) >>= g = m >>= (\x -> f x >>= g) -}
instance Monad []  where
  xs >>= f = [y | x <- xs, y <- f x]
  return x = [x]
instance  Monad Maybe  where
    (Just x) >>= k  = k x
    Nothing  >>= _  = Nothing
    return = Just
instance Monad (Either e) where
    Left  l >>= _ = Left l
    Right r >>= k = k r
instance Monad ((->) r) where
    f >>= k = \ r -> k (f r) r
instance Monoid a => Monad ((,) a) where
    (u, a) >>= k = case k a of (v, b) -> (u `mappend` v, b)
{- do notation is usually first encountered in the context of IO, but in fact it works for any monad.
do notation gets translated or "desugared" as follows:

do { a <- f ; m } = f >>= \a -> do { m }
do { f ; m } = f >> do { m }
do { m } = m

So this expression

do
  a <- f
  b <- g
  c <- h
  return (a, b, c)

 is equivalent to this.

f >>= \a ->
  g >>= \b ->
    h >>= \c ->
      return (a, b, c)
-}

example1 :: Maybe Int
example1 = Just 3 >>= \a ->
           Just 4 >>= \b ->
           return $ a + b

maybeSqrt x = if (x<0) then Nothing else Just (sqrt x)

example2 :: Maybe Double
example2 = maybeSqrt 9 >>= \a ->
           maybeSqrt 16 >>= \b ->
           return $ a + b
example3 :: Maybe Double
example3 = maybeSqrt (-9) >>= \a ->
           maybeSqrt 16 >>= \b ->
           return $ a + b
-- The Maybe monad is useful for modeling computations which may fail.
-- Note: no nested if statements!
example4 = sequence [Just 3, Just 4]
example5 = sequence [Just 3, Just 4, Nothing]

sequence :: Monad m => [m a] -> m [a]
sequence = foldr mcons (return [])

mcons :: Monad m => m t -> m [t] -> m [t]
mcons p q = p >>= \x ->
            q >>= \y ->
            return (x:y)
