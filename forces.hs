import Control.Arrow
import Data.Set

-- Each vertex must have an associated "charge" for repulsion.
-- Each edge must have a constant for springiness.

data Node = Node { charge :: Double
                 , position :: Position
                 } deriving (Eq, Show)

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

