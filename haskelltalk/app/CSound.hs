module CSound where

import Csound.Base
import Csound.Tab

import qualified Graphics.Gnuplot.Advanced as GP
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import Graphics.Gnuplot.Plot.TwoDimensional (linearScale)

{- The weierstrass function is everywhere continuous, but nowhere differentiable.
 It requires 0 < a < 1 and b an odd integer such that ab > 1 + 3/2*pi -}
weierstrassCoeffs :: (Num t1, Num t, Integral a) => t1 -> a -> [(t, t1)]
weierstrassCoeffs a b = map (\n -> (fromIntegral b^n, a^n)) [0..]

weierstrassSeq :: (Integral a, Floating b) => b -> a -> b -> [b]
--weierstrassSeq a b x = map (\n -> a^n*cos(b^n*pi*x)) [0..]
weierstrassSeq a b x = map (\(v,u) -> u*cos(v*pi*x)) $ weierstrassCoeffs a b

weierstrass :: (Floating a1, Integral a) => a1 -> a -> Int -> a1 -> a1
weierstrass a b n x = sum $ take n $ weierstrassSeq a b x

-- Now we can plot it to see what the weierstrass function looks like.

plotweierstrass :: Plot2D.T Double Double
plotweierstrass =
   Plot2D.function Graph2D.lines
      (linearScale 1000 (-2,2)) (weierstrass 0.9 7 100)

main :: IO ()
main = sequence_ [GP.plotDefault plotweierstrass]

-- We can also hear what it sounds like.
weierstrassTab :: Integral a => PartialStrength -> a -> Int -> Tab
weierstrassTab a b n = sines2 $ take n $ weierstrassCoeffs a b

wave :: Sig -> Sig
wave x = oscBy (weierstrassTab 0.9 7 1000) x

{- This will open up a virtual keyboard where you can hear what the weierstrass function sounds like. :) To close it, press Ctrl-C twice to return to ghci. -}
keyboard = vdac $ midi $ onMsg $ mul (fades 0.1 0.5) . wave
