module Satisfiability where

import Data.List (intercalate)

import Picosat (solve, solveAll)
import Picologic (Expr, readExpr, solveProp, ppSolutions)

import Data.Foldable (for_)
import Data.SBV (sInteger, constrain, allDifferent, allSat, inRange,  (.==), SInteger, SBool, Symbolic)

-- Basic boolean satisfiability; here we must encode the propositions as integers, etc.
--                    (A | ~B | C) & (B | D | E) & (D | F)
solutions = solveAll [[1 , -2 , 3] , [2 , 4 , 5] , [4 , 6]]

-- We can also use regular notation and read a proposition from a string.
solutions' = solveProp (readExpr "~(A | B)")  >>= putStrLn . ppSolutions

{- Now for some fun: Which answer is correct? (Thanks to David Webb for this!)
A. All of the below     
B. None of the below
C. All of the above
D. One of the above
E. None of the above
F. None of the above -}
a =      "(B & C & D & E & F)"
b =         "~(C | D | E | F)"
c =  "(A & B)"
d =  "(A | B | C)"
e = "~(A | B | C | D)"
f = "~(A | B | C | D | E)"
a' = logicxor a [b,c,d,e,f]
b' = logicxor b [a,c,d,e,f]
c' = logicxor c [a,c,d,e,f]
d' = logicxor d [a,b,c,e,f]
e' = logicxor e [a,b,c,d,f]
f' = logicxor f [a,b,c,d,e]
logicxor p props = p ++ " & ~( " ++ (intercalate " | " props) ++ " ) "
logicor  props = intercalate " | " props
logicand props = intercalate " & " props
proposition, proposition' :: Expr
proposition  = readExpr $ logicor [a ,b ,c, d, e, f]
proposition' = readExpr $ logicor [a',b',c',d',e',f']
answers  = solveProp proposition  >>= putStrLn . ppSolutions
answers' = solveProp proposition' >>= putStrLn . ppSolutions

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
bandaid = allSat puzzle
{-Solution #1:
  b = 4 :: Integer
  u = 1 :: Integer
  r = 5 :: Integer
  i = 9 :: Integer
  t = 7 :: Integer
  o = 0 :: Integer
  m = 8 :: Integer
  n = 3 :: Integer
  a = 2 :: Integer
  d = 6 :: Integer
This is the only solution. -}

-- | Find the value of a list of base 10 digits, i.e.
-- | 1234 = 1*10^3 + 2*10^2 + 3*10^1 + 4*10^0 = (((1)*10 + 2)*10 + 3)*10 + 4
val :: [SInteger] -> SInteger
val = foldr1 (\d r -> d + 10*r) . reverse
