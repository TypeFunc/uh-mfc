{-# LANGUAGE OverloadedStrings #-}
module Tidal where

import Sound.Tidal.Context
import Data.IORef

conf ::  IO (IORef
       (Double -> IO (), IO Rational, ParamPattern -> IO (),
        (Time -> [ParamPattern] -> ParamPattern) -> ParamPattern -> IO (),
        ParamPattern -> IO (),
        (Time -> [ParamPattern] -> ParamPattern) -> ParamPattern -> IO (),
        Double -> IO (), IO (), IO b -> IO b))
conf = do
      (cps, getNow) <- bpsUtils
      (d1,t1) <- superDirtSetters getNow
      (d2,t2) <- superDirtSetters getNow
      (d3,t3) <- superDirtSetters getNow
      (d4,t4) <- superDirtSetters getNow
      (d5,t5) <- superDirtSetters getNow
      (d6,t6) <- superDirtSetters getNow
      (d7,t7) <- superDirtSetters getNow
      (d8,t8) <- superDirtSetters getNow
      (d9,t9) <- superDirtSetters getNow
      (d10,t10) <- superDirtSetters getNow
      (c1,ct1) <- dirtSetters getNow
      (c2,ct2) <- dirtSetters getNow
      (c3,ct3) <- dirtSetters getNow
      (c4,ct4) <- dirtSetters getNow
      (c5,ct5) <- dirtSetters getNow
      (c6,ct6) <- dirtSetters getNow
      (c7,ct7) <- dirtSetters getNow
      (c8,ct8) <- dirtSetters getNow
      (c9,ct9) <- dirtSetters getNow
      (c10,ct10) <- dirtSetters getNow
      let bps x = cps (x/2)
      let hush = mapM_ ($ silence) [d1]
      let solo = (>>) hush
      newIORef (cps, getNow, d1, t1, d2, t2, bps, hush, solo)

hush conf = do
  it <- conf
  (cps, getNow, d1, t1, d2, t2, bps, hush', solo) <- readIORef it
  hush'
  return it

bdsn conf = do
  it <- conf
  (cps, getNow, d1, t1, d2, t2, bps, hush', solo) <- readIORef it
  d1 $ stack [sound "sn bd clap",
         note "2 3 5 7 11"]
  d2 $ sound "hh hh hh hh"
  return it

