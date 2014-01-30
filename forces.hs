import Control.Arrow

-- Each vertex must have an associated "charge" for repulsion.
-- Each edge must have a constant for springiness.

data Node = Node { charge :: Double
                 , position :: Position
                 } deriving (Equals, Show)

data Spring = Spring { stiffness :: Double
                     , nodes :: (Node, Node)
                     } deriving (Equals, Show)

type Force = (Double, Double)
type Position = (Double, Double)
