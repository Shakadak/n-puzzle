module AStar (aStarSearch) where
import Data.HashPSQ as PSQ
import Data.Hashable
import Data.Map     as Map
import Data.Set     as Set
import Data.List    (unfoldr)
import Data.Maybe   (fromMaybe)

data AStar a cost = AStar {
    expand    :: a -> Set a,
    goal      :: a -> Bool,
    opened    :: PSQ.HashPSQ a cost a,
    closed    :: Set.Set a,
    distances :: Map.Map a cost,
    path      :: Map.Map a a,
    heuristic :: a -> cost,
    cost      :: a -> a -> cost }

aStarSearch
  :: (Num cost, Ord a, Ord cost, Hashable a) =>
     (a -> Set a)       -- expand
  -> (a -> a -> cost)   -- cost
  -> (a -> cost)        -- heuristic
  -> (a -> Bool)        -- goal
  -> a                  -- start
  -> Maybe [a]          -- Maybe Path
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

runAStar aStar = do
    (k, p, current, o) <- PSQ.minView (opened aStar)
    if goal aStar current
       then return $ backtrack (path aStar) current
       else let aStar' = aStar {opened = o, closed = Set.insert current (closed aStar)}
             in runAStar $ fst (Set.foldr eval (aStar', (current, fromMaybe 0 (Map.lookup current $ distances aStar'))) (expand aStar' current))

eval n (aStar, t@(parent, f)) =
    let fn = f + cost aStar parent n
        o = opened    aStar
        c = closed    aStar
        p = path      aStar
        d = distances aStar
        g = heuristic aStar in
        case (PSQ.lookup n o, Set.member n c, Map.lookup n p, Map.lookup n (distances aStar)) of
          (Nothing, False, _,      _)                   -> (aStar
            { opened    = PSQ.insert n (fn + g n) n o
            , path      = Map.insert n parent p
            , distances = Map.insert n fn d },
              t)
          (Nothing, True,  Just _, Just fp) | fn < fp   -> (aStar
            { opened = PSQ.insert n (fn + g n) n o
            , closed = Set.delete n            c
            , path   = Map.insert n parent p
            , distances = Map.insert n fn d },
              t)
          (Just _,  _,     Just _, Just fp) | fn < fp   -> (aStar
            { opened = PSQ.insert n (fn + g n) n o
            , path   = Map.insert n parent p
            , distances = Map.insert n fn d },
              t)
          _                                             -> (aStar, t)

backtrack paths goal = reverse $ goal:unfoldr (\x -> fmap (\x -> (x, x)) (Map.lookup x paths)) goal
