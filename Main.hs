import System.Environment
import Data.List
--import Data.Maybe
import AStar
import Parser
import NPuzzle
import Heuristics

main :: IO ()
main = do args <- getArgs
          if length args /= 1
          then putStrLn "Usage: ./graphi file/path"
          else do
              content <- readFile (head args)
              case parseGrid (head args) content of
                Left err -> print err
                Right grid -> do putStr content
                                 putStrLn "--solution--"
                                 let goal = generateGoal $ intRoot $ length grid
                                 let path = aStarSearch expand cost (manhattan goal) (== goal) (toArray grid)
                                 putStr $ maybe "No path available\n" showPath path

showPath :: [Grid] -> String
showPath = foldl' (\str state -> str ++ showGrid state ++ "\n") ""

intRoot :: Int -> Int
intRoot = truncate . sqrt . (fromIntegral :: Int -> Double)
