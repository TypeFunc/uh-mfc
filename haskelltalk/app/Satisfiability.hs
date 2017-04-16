module Satisfiability where

import Data.Foldable (for_)
import Data.SBV (sInteger, constrain, allDifferent, allSat, inRange,  (.==), SInteger, SBool, Symbolic)

-- | Find the value of a list of base 10 digits, i.e.
-- | 1234 = 1*10^3 + 2*10^2 + 3*10^1 + 4*10^0 = (((1)*10 + 2)*10 + 3)*10 + 4
val :: [SInteger] -> SInteger
val = foldr1 (\d r -> d + 10*r) . reverse

{- Question: Is it possible to assign different digits to the following letters
such that the equation burito + monad = bandaid is satisfied? -}
puzzle :: Symbolic SBool
puzzle = do
  ds@[b,u,r,i,t,o,m,n,a,d] <- sequenceA [ sInteger [v] | v <- "buritomnad" ]
  constrain $ allDifferent ds
  for_ ds $ \d -> constrain $ inRange d (0,9)
  pure $    val [b,u,r,r,i,t,o]
          + val     [m,o,n,a,d]
        .== val [b,a,n,d,a,i,d]

{- Answer: Yep! -}
solution = allSat puzzle
