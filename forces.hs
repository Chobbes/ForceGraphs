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

import Data.Functor
import Data.Map

-- Each vertex must have an associated "charge" for repulsion.
data Node = Node { charge :: Double
                 , position :: Position
                 } deriving (Eq, Show)

-- Each edge is a spring
data Spring = Spring { stiffness :: Double
                     , springNode :: Node
                     } deriving (Eq, Show)

-- Each edge must have a constant for springiness.
data Graph = Graph { graphVerts :: [Node]
                   , edges :: Map Node [Spring]
                   } deriving (Eq, Show)

type Force = Vec2 Double
type Position = Vec2 Double


data Vec2 a = Vec2 a a deriving (Eq, Show)

instance (Num a) => Num (Vec2 a) where
  (+) (Vec2 a b) (Vec2 c d) = Vec2 (a + c) (b + d)
  (-) (Vec2 a b) (Vec2 c d) = Vec2 (a - c) (b - d)
  (*) (Vec2 a b) (Vec2 c d) = Vec2 (a * c) (b * d)
  abs = fmap abs
  signum = fmap signum
  fromInteger x = fromIntegral <$> (Vec2 x x)

instance Functor Vec2 where
  fmap f (Vec2 a b) = Vec2 (f a) (f b)

-- Dot product of a 2D vector
(|.|) :: (Num a) => Vec2 a -> Vec2 a -> a
(|.|) u v = let Vec2 a b = u * v in a + b

-- Get the magnitude of a tuple (vector magnitude).
magnitude :: (Floating a) => Vec2 a -> a
magnitude v = sqrt $ v |.| v

-- Returns a tuple with a magnitude of 1 (barring floating point errors).
normalize :: (Floating a) => Vec2 a -> Vec2 a
normalize v@(Vec2 x y) = let mag = magnitude v in Vec2 (x / mag) (y / mag)

-- Force of the second node on the first node.
repulsiveForce :: Node -> Node -> Force
repulsiveForce u v = (*force) <$> dir
    where dir = normalize (position v - position u)
          dist = magnitude (position v - position u)
          force = negate $ charge v * charge u / dist**2

-- Force of a spring on a node.
attractiveForce :: Node -> Spring -> Force
attractiveForce Node {position=u} spring@(Spring {stiffness=k}) = (*k) <$> (v - u)
  where v = position $ springNode spring
