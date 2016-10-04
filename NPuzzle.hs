module NPuzzle where
import Data.Array
import Data.Maybe
import Data.List
import Data.Tuple
import Data.Set (Set, fromList)
import Data.Ord
import Data.Function
import Data.Hashable

data Direction = Up | Dn | Lt | Rt
type Coord = (Int, Int)
type Grid = Array Int Coord

solution :: Int -> [[Int]]
solution = fst . go
    where start n = ([], 0:[n ^ 2 - 1, n ^ 2 - 2..1]) :: ([[Int]], [Int])
          rotateClock = transpose . reverse
          update (s, d) n = (reverse (take n d) : rotateClock s, drop n d)
          go n = foldl' update (start n) (fmap (`div` 2) [2..2*n])

toArray :: [Int] -> Grid
toArray grid = array (0, length grid - 1) (zip grid [(y, x) | y <- [0..n], x <- [0..n]])
    where n = truncate (sqrt $ fromIntegral $ length grid) - 1

cost :: Num b => a -> a -> b
cost _ _ = 1

generateGoal :: Int -> Grid
generateGoal = toArray . concat . solution

expand :: Grid -> Set Grid
expand state = {-# SCC "expand" #-} fromList $ maybe [] pure . gridSwap state =<< [Up, Dn, Lt, Rt]

gridSwap :: Grid -> Direction -> Maybe Grid
gridSwap grid dir = do
    let movedTile = moveTile (grid!0) dir
    ind <- getIndex grid movedTile
    pure $ grid//[(0, movedTile), (ind, grid!0)]

getIndex :: Grid -> Coord -> Maybe Int
getIndex grid coord = iLookup coord (assocs grid)

moveTile :: Coord -> Direction -> Coord
moveTile (y, x) Up = (y - 1, x    )
moveTile (y, x) Dn = (y + 1, x    )
moveTile (y, x) Lt = (y    , x - 1)
moveTile (y, x) Rt = (y    , x + 1)

iLookup :: Eq b => b -> [(a, b)] -> Maybe a
iLookup x = foldl' (\acc (i, x') -> if isNothing acc && x == x' then Just i else acc) Nothing

showGrid :: Grid -> String
showGrid grid = concatMap ((++ "\n") . unwords . map (fill . show)) nums
    where
        col = length . show . snd . bounds $ grid
        fill s = replicate (col - length s) ' ' ++ s
        nums = map (map fst) . groupBy ((==) `on` fst.snd) . sortBy (comparing snd) . assocs $ grid

gridIndices :: Grid -> [Int]
gridIndices = map fst . sortBy (comparing snd) . assocs

instance (Hashable e, Ix i) => Hashable (Array i e) where
    hashWithSalt = foldl' hashWithSalt
