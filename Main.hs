import System.Environment
import Data.Tuple
import Data.List
import Data.Maybe
import Data.Set (fromList)
import Graph
import AStar
import Parser

main = do args <- getArgs
          if length args /= 1
          then putStrLn "Usage: ./graphi file/path"
          else do
              content <- readFile (head args)
              case parseGrid (head args) content of
                Left err -> print err
                Right grid -> do putStr content
                                 putStrLn "--solution--"
                                 putStr $ maybe "No path available\n" showPath $ aStarSearch (expand graph) cost (manhattan' goal) (== goal) grid

showPath = foldl' (\str (x, y) -> str ++ show (truncate x) ++ "," ++ show (truncate y) ++ "\n") ""

expand xs x = fromList . fromMaybe [] $ lookup x xs

manhattan (gx, gy) (cx, cy) = abs (gx - cx) + abs (gy - cy)

manhattan' (gx, gy) (cx, cy) = sqrt $ (gx - cx) ^ 2 + (gy - cy) ^ 2

readMany = unfoldr $ listToMaybe . concatMap reads . tails
