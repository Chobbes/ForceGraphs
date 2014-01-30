import Control.Arrow

-- Each vertex must have an associated "charge" for repulsion.
-- Each edge must have a constant for springiness.

data Node = Node { charge :: Double
                 , position :: Position
                 } deriving (Eq, Show)

data Spring = Spring { stiffness :: Double
                     , nodes :: (Node, Node)
                     } deriving (Eq, Show)

type Force = (Double, Double)
type Position = (Double, Double)
