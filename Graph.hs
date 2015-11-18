import Data.List
import Data.Function
import Data.Ord
import Control.Arrow

type Graph node = [(node, [node])]

fromEdges :: (Eq node, Ord node) => [(node, node)] -> Graph node
fromEdges = map shave . groupBy ((==) `on` fst) . sortBy (comparing fst)
    where shave = (head *** id) . unzip
