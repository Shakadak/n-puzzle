{-# LANGUAGE BangPatterns #-}
module NPuzzleList where
import Data.List
import Data.Tuple
import Data.Set (Set, fromList)
import Data.Ord
import Data.Function
import Safe

data Direction = Up | Dn | Lt | Rt
type Coord = (Int, Int)
type Grid = [Coord]

solution = fst . go
    where start n = ([], 0:[n ^ 2 - 1, n ^ 2 - 2..1])
          rotateClock = transpose . reverse
          update (s, d) n = (reverse (take n d) : rotateClock s, drop n d)
          go n = foldl' update (start n) (fmap (`div` 2) [2..2*n])

toArray :: [Int] -> Grid
toArray grid = map snd . groupBy (comparing fst) . zip $ grid [(y, x) | y <- [0..n], x <- [0..n]]
    where n = truncate (sqrt $ fromIntegral $ length grid) - 1

cost one step = 1

generateGoal :: Int -> Grid
generateGoal = {-toArray .-} concat . solution

expand :: Grid -> Set Grid
expand state = {-# SCC "expand" #-} fromList $ maybe [] pure . gridSwap state =<< [Up, Dn, Lt, Rt]

gridSwap :: Grid -> Direction -> Maybe Grid
gridSwap grid dir = do
    let movedTile = moveTile (at 0 grid) dir
    ind <- elemIndex movedTile grid
    return $ unionBy ((==) `on` fst) [(0, movedTile), (ind, at 0 grid)] grid

--getIndex :: Grid -> Coord -> Maybe Int
--getIndex grid coord = lookup coord (stateToCoords grid)

moveTile :: Coord -> Direction -> Coord
moveTile (y, x) Up = (y - 1, x    )
moveTile (y, x) Dn = (y + 1, x    )
moveTile (y, x) Lt = (y    , x - 1)
moveTile (y, x) Rt = (y    , x + 1)

stateToCoords :: Grid -> [(Coord, Int)]
stateToCoords = map swap

showGrid :: Grid -> String
showGrid grid = concatMap ((++ "\n") . intercalate "  " . map show) nums
    where
        col = (length . show $ (length grid - 1)) + 1
        nums = map (map fst) . groupBy ((==) `on` fst.snd) . sortBy (comparing snd) $ grid
