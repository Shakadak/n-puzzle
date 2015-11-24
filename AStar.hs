module AStar (aStarSearch) where
-- import Data.PSQueue as PSQ
import Data.OrdPSQ  as PSQ
import Data.Map     as Map
import Data.Set     as Set
import Data.List    (unfoldr)
import Data.Maybe   (fromMaybe)

data AStar a cost = AStar {
    expand    :: a -> Set a,
    goal      :: a -> Bool,
    opened    :: PSQ.OrdPSQ a cost a,
    closed    :: Set.Set a,
    distances :: Map.Map a cost,
    path      :: Map.Map a a,
    heuristic :: a -> cost,
    cost      :: a -> a -> cost }

aStarSearch expand cost heuristic goal start =
    runAStar AStar
    { expand    = expand
    , opened    = PSQ.singleton start (heuristic start) start
    , closed    = Set.empty
    , path      = Map.empty
    , heuristic = heuristic
    , goal      = goal
    , distances = Map.empty
    , cost      = cost }

runAStar :: (Ord a, Ord cost, Num cost) => AStar a cost -> Maybe [a]
runAStar aStar = do
    (k, p, current, o) <- ({-# SCC "PSQ.minView" #-} PSQ.minView (opened aStar))
    if goal aStar current
       then return $ backtrack (path aStar) current
       else let aStar' = aStar {opened = o, closed = Set.insert current (closed aStar)}
             in runAStar $ fst (Set.foldr eval (aStar', (current, fromMaybe 0 (Map.lookup current $ distances aStar'))) (expand aStar' current))

eval :: (Ord a, Ord cost, Num cost) => a -> (AStar a cost, (a, cost)) -> (AStar a cost, (a, cost))
eval n (aStar, t@(parent, f)) = {-# SCC "eval" #-}
    let fn = f + cost aStar parent n
        o = opened    aStar
        c = closed    aStar
        p = path      aStar
        d = distances aStar
        g = heuristic aStar in
        case (({-# SCC "PSQ.lookup" #-} PSQ.lookup n o), ({-# SCC "Set.Member" #-} Set.member n c), ({-# SCC "Map.lookupPath" #-} Map.lookup n p), ({-# SCC "Map.lookupDistance" #-} Map.lookup n (distances aStar))) of
          (Nothing, False, _,      _)                   -> (aStar
            { opened    = ({-# SCC "PSQ.insert1" #-} PSQ.insert n (fn + g n) n o)
            , path      = ({-# SCC "Map.insertPath1" #-} Map.insert n parent p)
            , distances = ({-# SCC "Map.insertDistance1" #-} Map.insert n fn d) },
              t)
          (Nothing, True,  Just _, Just fp) | fn < fp   -> (aStar
            { opened = ({-# SCC "PSQ.insert2" #-} PSQ.insert n (fn + g n) n o)
            , closed = Set.delete n            c
            , path   = ({-# SCC "Map.insertPath2" #-} Map.insert n parent p)
            , distances = ({-# SCC "Map.insertDistance2" #-} Map.insert n fn d) },
              t)
          (Just _,  _,     Just _, Just fp) | fn < fp   -> (aStar
            { opened = ({-# SCC "PSQ.adjust" #-} PSQ.insert n (fn + g n) n o)
            , path   = ({-# SCC "Map.insertPath3" #-} Map.insert n parent p)
            , distances = ({-# SCC "Map.insertDistance3" #-} Map.insert n fn d) },
              t)
          _                                             -> (aStar, t)

backtrack paths goal = reverse $ goal:unfoldr (\x -> fmap (\x -> (x, x)) (Map.lookup x paths)) goal
