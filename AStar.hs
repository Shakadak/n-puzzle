module AStar (aStarSearch) where
import Data.HashPSQ as PSQ
import Data.Hashable
import Data.HashMap     as Map
import Data.Set     as Set
import Data.List    (unfoldr)
import Data.Maybe   (fromMaybe)

data AStar a cost = AStar {
    expand    :: a -> Set a,
    goal      :: a -> Bool,
    opened    :: PSQ.HashPSQ a cost a,
    closed    :: Map.Map a (),
    distances :: Map.Map a cost,
    path      :: Map.Map a a,
    heuristic :: a -> cost,
    cost      :: a -> a -> cost }

aStarSearch :: (Num cost, Ord a, Ord cost, Hashable a) =>
     (a -> Set a)       -- expand
  -> (a -> a -> cost)   -- cost
  -> (a -> cost)        -- heuristic
  -> (a -> Bool)        -- goal
  -> a                  -- start
  -> Maybe [a]          -- Maybe Path
aStarSearch expand' cost' heuristic' goal' start =
    runAStar AStar
        { expand    = expand'
        , opened    = PSQ.singleton start (heuristic' start) start
        , closed    = Map.empty
        , path      = Map.empty
        , heuristic = heuristic'
        , goal      = goal'
        , distances = Map.empty
        , cost      = cost' }

runAStar :: (Num cost, Ord a, Ord cost, Hashable a) =>
    AStar a cost -> Maybe [a]
runAStar aStar = do
    (_, _, current, o) <- PSQ.minView (opened aStar)
    if goal aStar current
       then return $ backtrack (path aStar) current
       else let aStar' = aStar {opened = o
                               , closed = Map.insert current () (closed aStar)}
                neighbors = expand aStar' current
                f = fromMaybe 0 (Map.lookup current $ distances aStar')
             in runAStar $ fst (Set.foldr eval (aStar', (current, f)) neighbors)

eval :: (Num cost, Ord a, Ord cost, Hashable a) =>
    a -> (AStar a cost, (a, cost)) -> (AStar a cost, (a, cost))
eval n (aStar, t@(parent, f)) =
    let fn = f + cost aStar parent n
        o = opened    aStar
        c = closed    aStar
        p = path      aStar
        d = distances aStar
        g = heuristic aStar in
        case (PSQ.lookup n o, Map.member n c, Map.lookup n p, Map.lookup n (distances aStar)) of
          (Nothing, False, _,      _)                   -> (aStar
            { opened    = PSQ.insert n (fn + g n) n o
            , path      = Map.insert n parent p
            , distances = Map.insert n fn d },
              t)
          (Nothing, True,  Just _, Just fp) | fn < fp   -> (aStar
            { opened = PSQ.insert n (fn + g n) n o
            , closed = Map.delete n            c
            , path   = Map.insert n parent p
            , distances = Map.insert n fn d },
              t)
          (Just _,  _,     Just _, Just fp) | fn < fp   -> (aStar
            { opened = PSQ.insert n (fn + g n) n o
            , path   = Map.insert n parent p
            , distances = Map.insert n fn d },
              t)
          _                                             -> (aStar, t)

backtrack :: (Ord a, Hashable a) =>
    Map a a -> a -> [a]
backtrack paths end = reverse $ end:unfoldr (\s -> fmap (\x -> (x, x)) (Map.lookup s paths)) end
