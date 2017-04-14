{-# LANGUAGE OverloadedStrings #-}
{- The Overloaded Strings language extension is quite handy because it allows you to write text without using 'fromString' everywhere. -}

module HaTeX where

import AD

import Text.LaTeX (LaTeXT, LaTeXT_, document, documentclass, author, article, title, maketitle, section, hatex, textbf, large, execLaTeXT, fromString, newline, (<>), renderFile, Texy, texy, center, usepackage, matrixTabular, emph, Measure(Pt))
import Text.LaTeX.Packages.Inputenc
import Data.Matrix (fromList, matrix)
import Text.LaTeX.Packages.TikZ (draw, bpath, line, tikz, tikzpicture, pointAtXY, TPath(Start), (->>), (->-))

simple :: Monad m => LaTeXT_ m
simple = do
 simplePreamble
 document simpleBody

simplePreamble :: Monad m => LaTeXT_ m
simplePreamble = do
 documentclass [] article
 usepackage [utf8] inputenc
 usepackage [] tikz
 author "Jake Fennick"
 title "Automatic Differentiation with AD"

simpleBody :: Monad m => LaTeXT_ m
simpleBody = do
 maketitle
 
 section "Function"
 center $ tikzpicture $
      draw (Start (pointAtXY   0    1) ->- pointAtXY  0     (-1))
  ->> draw (Start (pointAtXY (-0.0) 0) ->- pointAtXY (2*pi)   0 )
  ->> (draw $ bpath (pointAtXY 0 0) $
        mapM_ line [ pointAtXY x (sines 6 x) | x <- [0,0.05 .. 2*pi] ])
 center $ fromString $ show sines4
 
 section "Derivatives"
 let showline x = (fromString . show $ x) <> newline <> newline
 sequence $ map showline $ tail $ nth_derivatives 3
 
 "Generated with the "
 hatex
 " library."

-- By executing 'execLaTeXT' you run the 'LaTeXT' monad and make a 'LaTeX' value as output.
-- With 'renderFile' you render it to 'Text' and write it in a file.
makesimple :: IO ()
makesimple = execLaTeXT simple >>= renderFile "simple.tex"
{- Note: To convert the .tex file to .pdf, you can use pdflatex.  You will need to install texlive.
On ubuntu, enter "sudo apt-get install texlive" -}
