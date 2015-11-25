module Heuristics where
import Data.Array
import Data.List
import NPuzzle
--import NPuzzleList
import Safe


{-manhattanList :: NPuzzleList.Grid -> NPuzzleList.Grid -> Int
manhattanList end start = {-# SCC "manhattan" #-} sum . map (go . fst) $ start
    where
        manhattan' (gx, gy) (cx, cy) = abs (gx - cx) + abs (gy - cy)
        go i = manhattan' ({-# SCC "end!i" #-} lookupJust i end) ({-# SCC "start!i" #-} lookupJust i start)
-}

manhattan :: NPuzzle.Grid -> NPuzzle.Grid -> Int
manhattan end start = {-# SCC "manhattan" #-} sum $ map go (indices start)
    where
        manhattan' (gx, gy) (cx, cy) = abs (gx - cx) + abs (gy - cy)
        go i = manhattan' ({-# SCC "end!i" #-} end!i) ({-# SCC "start!i" #-} start!i)
