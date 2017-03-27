{-# LANGUAGE OverloadedStrings #-}
{- The Overloaded Strings language extension is quite handy because it allows you to write text without using 'fromString' everywhere. -}

module HaTeX where

import Text.LaTeX (LaTeXT, LaTeXT_, document, documentclass, author, article, title, maketitle, section, hatex, textbf, large, execLaTeXT, renderFile, Texy, texy, center, usepackage, matrixTabular, emph, Measure(Pt))
import Text.LaTeX.Packages.Inputenc
import Data.Matrix (fromList, matrix)
import Text.LaTeX.Packages.TikZ --(pointAtXY)

--simple example
simple :: Monad m => LaTeXT_ m
simple = do
 simplePreamble
 document simpleBody

simplePreamble :: Monad m => LaTeXT_ m
simplePreamble = do
 documentclass [] article
 author "Daniel Diaz"
 title "Simple example"

simpleBody :: Monad m => LaTeXT_ m
simpleBody = do
 maketitle
 section "Hello"
 "This is a simple example using the "
 hatex
 " library. "
 textbf "Enjoy!"
 " "
 textbf (large "Yoohoo!")

-- By executing 'execLaTeXT' you run the 'LaTeXT' monad and make a 'LaTeX' value as output.
-- With 'renderFile' you render it to 'Text' and write it in a file.
makesimple :: IO ()
makesimple = execLaTeXT simple >>= renderFile "simple.tex"
{- Note: To convert the .tex file to .pdf, you can use pdflatex.  You will need to install texlive.
On ubuntu, enter "sudo apt-get install texlive" -}

--tables example
tables :: Monad m => LaTeXT m ()
tables = tablePreamble >> document tableBody

tablePreamble :: Monad m => LaTeXT m ()
tablePreamble = do
  documentclass [] article
  usepackage [utf8] inputenc
  author "Daniel Díaz"
  title "Examples of Tables"

tableBody :: Monad m => LaTeXT m ()
tableBody = do
  maketitle
  -- Table from a simple matrix
  center $ matrixTabular (fmap textbf ["x","y","z"]) $
    fromList 3 3 [ (1 :: Int)..]
  -- Table from a matrix calculated in-place
  center $ matrixTabular (fmap textbf ["Number","Square root"]) $
    matrix 9 2 $ \(i,j) -> if j == 1 then I i else R $ sqrt $ fromIntegral i

-- Creating custom instances of Texy to display elements within a table.
data Number = R Double | I Int

instance Texy Number where
  texy (R x) = texy x
  texy (I i) = texy i

maketables :: IO ()
maketables = execLaTeXT tables >>= renderFile "tables.tex"


--TikZ graphics example

tikztest :: LaTeXT IO ()
tikztest = do
 tikzPreamble --{-# SCC "" #-} 
 document tikzBody

tikzPreamble :: LaTeXT IO ()
tikzPreamble = do
 documentclass [] article
 usepackage [utf8] inputenc
 usepackage [] tikz
 author "Daniel Díaz"
 title "Example using TikZ"

tikzBody :: LaTeXT IO ()
tikzBody = do
 maketitle
 "Below a picture generated using the TikZ DSL of "
 hatex
 "."
 center $ tikzpicture $ draw $
  Cycle $ Start (pointAtXY 0 0) ->- pointAtXY 1 0 ->- pointAtXY 0 1
 "And some pictures more."
 center $ tikzpicture $
      draw  (Rectangle (Start $ pointAtXY 0   0  ) (pointAtXY 1 1))
  ->> fill  (Circle    (Start $ pointAtXY 1.5 0.5)  0.5)
  ->> shade (Ellipse   (Start $ pointAtXY 3   0.5 ) 1 0.5)
 center $ tikzpicture $ draw $
  (Cycle $ Start (pointAtXY 0 0) ->- pointAtXY 1 0 ->- pointAtXY 0 1) ->- pointAtXY 1 1
 "We also show the graph of the "
 emph "sine"
 " function."
 center $ tikzpicture $
      draw (Start (pointAtXY   0    1) ->- pointAtXY  0     (-1))
  ->> draw (Start (pointAtXY (-0.2) 0) ->- pointAtXY (3*pi)   0 )
  ->> scope [TColor $ BasicColor Blue, TWidth (Pt 1)] (draw $ bpath (pointAtXY 0 0) $
        mapM_ line [ pointAtXY x (sin x) | x <- [0,0.05 .. 3*pi] ])

maketikz :: IO ()
maketikz = execLaTeXT tikztest >>= renderFile "tikz.tex"
