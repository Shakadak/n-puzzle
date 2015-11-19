module Graph where
import Data.List
import Data.Function
import Data.Ord
import Control.Arrow

fromEdges :: (Eq node, Ord node) => [(node, node)] -> [(node, [node])]
fromEdges = map shave . groupBy ((==) `on` fst) . sortBy (comparing fst)
    where shave = first head . unzip
