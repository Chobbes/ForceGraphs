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
import Data.Map hiding (filter, map, foldl)
import Prelude hiding (lookup)
import Data.Maybe

-- Each vertex must have an associated "charge" for repulsion.
data Node = Node { charge :: Double
                 , position :: Position
                 } deriving (Eq, Ord, Show)

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

instance (Ord a) => Ord (Vec2 a) where
  (<=) (Vec2 a b) (Vec2 c d) = a < c || (a == c && b <= d)

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

-- Force for a node in a graph.
nodeForce :: Graph -> Node -> Force
nodeForce Graph {edges=es, graphVerts=vs} node = attract + repulse
  where attract = sum $ map (attractiveForce node) (fromMaybe [] $ lookup node es)
        repulse = sum $ map (repulsiveForce node) (filter (/=node) vs)

-- Forces for all nodes in a graph.
graphForces :: Graph -> Map Node Force
graphForces g = foldl union empty $ map (\n -> singleton n (nodeForce g n)) (graphVerts g)

-- Update graph. Returns the total distance moved by all verts as well as the new graph.
-- Each step assumes a mass of 1, and a timestep of 1.
graphUpdate :: Graph -> (Double, Graph)
graphUpdate g@(Graph {edges=es, graphVerts=vs}) = (dist, Graph newVerts es)
  where newVerts = map (\n@(Node c v) -> Node c (v + (fromMaybe 0 $ lookup n forces))) vs
        forces = graphForces g
        dist = sum $ map magnitude $ zipWith (\n1 n2 -> position n1 - position n2) newVerts vs

-- Update the graph until there is less than a certain amount of change.
graphFullUpdate :: Graph -> Double -> Graph
graphFullUpdate g error
  | dist < error = newG
  | otherwise = graphFullUpdate newG error
  where (dist, newG) = graphUpdate g
