module Heuristics where
import Data.Array
import Data.List
import NPuzzle


manhattan :: Grid -> Grid -> Int
manhattan end start = sum $ map go (indices start)
    where
        manhattan' (gx, gy) (cx, cy) = abs (gx - cx) + abs (gy - cy)
        go i = manhattan' (end!i) (start!i)
