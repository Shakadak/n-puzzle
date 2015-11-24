module NPuzzle where
import Data.Array
import Data.List
import Data.Tuple

data Direction = Up | Dn | Lt | Rt
type Coord = (Int, Int)
type Grid = Array Int Coord

solution size = fst $ go' size
    where start n = ([], 0:[n ^ 2 - 1, n ^ 2 - 2..1])
          rotateClock = transpose . reverse
          update (s, d) n = (reverse (take n d) : rotateClock s, drop n d)
          go' n = foldl' update (start n) (fmap (`div` 2) [2..2*n])

toArray :: [Int] -> Grid
toArray grid = array (0, length grid - 1) (zip grid [(y, x) | y <- [0..n], x <- [0..n]])
    where n = truncate (sqrt $ fromIntegral $ length grid) - 1

cost one step = 1

expand state = map (gridSwap state) [Up, Dn, Lt, Rt] >>= maybe [] pure

gridSwap :: Grid -> Direction -> Maybe Grid
gridSwap grid dir = do
    let movedTile = moveTile (grid!0) dir
    ind <- getIndex grid movedTile
    return $ grid//[(0, movedTile), (ind, grid!0)]

getIndex :: Grid -> Coord -> Maybe Int
getIndex grid coord = lookup coord (map swap $ assocs grid)

moveTile :: Coord -> Direction -> Coord
moveTile (y, x) Up = (y - 1, x    )
moveTile (y, x) Dn = (y + 1, x    )
moveTile (y, x) Lt = (y    , x - 1)
moveTile (y, x) Rt = (y    , x + 1)
