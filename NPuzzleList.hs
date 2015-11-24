module NPuzzleList where
import Data.List
import Data.Tuple
import Data.Set (Set, fromList)
import Data.Ord
import Data.Function

data Direction = Up | Dn | Lt | Rt
type Coord = (Int, Int)
type Grid = [[Int]]
