{- Copyright (C) 2013 Calvin Beck

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation files
   (the "Software"), to deal in the Software without restriction,
   including without limitation the rights to use, copy, modify, merge,
   publish, distribute, sublicense, and/or sell copies of the Software,
   and to permit persons to whom the Software is furnished to do so,
   subject to the following conditions:
   
   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.
   
   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
-}

import Control.Arrow
import Data.Set

-- Each vertex must have an associated "charge" for repulsion.
data Node = Node { charge :: Double
                 , position :: Position
                 } deriving (Eq, Show)

-- Each edge must have a constant for springiness.
data Graph = Graph { graphVerts :: [Node]
                   , edges :: Set (Double, Node)
                   } deriving (Eq, Show)

type Force = (Double, Double)
type Position = (Double, Double)

-- Get the magnitude of a tuple (vector magnitude).
magnitude :: (Double, Double) -> Double
magnitude = sqrt . tupleSum . ((**2) *** (**2))
  where tupleSum t = fst t + snd t

-- Returns a tuple with a magnitude of 1 (barring floating point errors).
normalize :: (Double, Double) -> (Double, Double)
normalize t@(x, y) = let mag = magnitude t in (x / mag, y / mag)

