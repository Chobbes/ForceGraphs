-- Each vertex must have an associated "charge" for repulsion.
-- Each edge must have a constant for springiness.

data Node = Node { charge :: Double
                 , position :: (Double, Double)
                 } deriving (Equals, Show)

data Spring = Spring { stiffness :: Double
                     , nodes :: (Node, Node)
                     } deriving (Equals, Show)

newtype Force = (Double, Double)
