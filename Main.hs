import System.Environment
import Data.Tuple
import Data.List
import Data.Maybe
import Data.Set (fromList)
import Graph
import AStar

main = do args <- getArgs
          content <- readFile (head args)
          let (edges, [s,g]) = span ((== 4) . length) . filter (not . null) . map (readMany :: String -> [Int]) $ lines content
          let graph = fromEdges . bidir . map cnv $ edges
              start = fromCoord s
              goal = fromCoord g
          putStr content
          putStrLn "--solution--"
          putStr $ maybe "No path available\n" showPath $ aStarSearch (expand graph) cost (manhattan goal) (== goal) start

showPath = foldl' (\str (x, y) -> str ++ show x ++ "," ++ show y ++ "\n") ""

expand xs x = fromList . fromMaybe [] $ lookup x xs

cost _ _ = 1

manhattan (gx, gy) (cx, cy) = abs (gx - cx) + abs (gy - cy)

fromCoord (x:y:_) = (x, y)

cnv [a,b,c,d] = ((a, b), (c, d))

bidir = concatMap (\x -> [x, swap x])

readMany = unfoldr $ listToMaybe . concatMap reads . tails
