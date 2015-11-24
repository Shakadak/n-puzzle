module Heuristics where
import Data.Array
import Data.List
import NPuzzle


manhattan :: Grid -> Grid -> Int
manhattan end start = foldl' go 0 (indices start)
    where
        manhattan' (gx, gy) (cx, cy) = abs (gx - cx) + abs (gy - cy)
        go acc i = acc + manhattan' (end!i) (start!i)
