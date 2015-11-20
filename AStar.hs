module AStar (aStarSearch) where
import Data.PSQueue as PSQ
import Data.Map     as Map
import Data.Set     as Set
import Data.List    (unfoldr)
import Data.Maybe   (fromMaybe)

data AStar a cost = AStar {
    expand    :: a -> Set a,
    goal      :: a -> Bool,
    opened    :: PSQ.PSQ a cost,
    closed    :: Set.Set a,
    distances :: Map.Map a cost,
    path      :: Map.Map a a,
    heuristic :: a -> cost,
    cost      :: a -> a -> cost }

aStarSearch expand cost heuristic goal start =
    runAStar AStar
    { expand    = expand
    , opened    = PSQ.singleton start (heuristic start)
    , closed    = Set.empty
    , path      = Map.empty
    , heuristic = heuristic
    , goal      = goal
    , distances = Map.empty
    , cost      = cost }

runAStar :: (Ord a, Ord cost, Num cost) => AStar a cost -> Maybe [a]
runAStar aStar = do
    (current :-> _, o) <- PSQ.minView (opened aStar)
    if goal aStar current
       then return $ backtrack (path aStar) current
       else let aStar' = aStar {opened = o, closed = Set.insert current (closed aStar)}
             in runAStar $ fst (Set.foldr eval (aStar', (current, fromMaybe 0 (Map.lookup current $ distances aStar'))) (expand aStar' current))

eval :: (Ord a, Ord cost, Num cost) => a -> (AStar a cost, (a, cost)) -> (AStar a cost, (a, cost))
eval n (aStar, t@(parent, f)) =
    let fn = f + cost aStar parent n
        o = opened    aStar
        c = closed    aStar
        p = path      aStar
        d = distances aStar
        g = heuristic aStar in
        case (PSQ.lookup n o, Set.member n c, Map.lookup n p, Map.lookup n (distances aStar)) of
          (Nothing, False, _,      _)                   -> (aStar
            { opened    = PSQ.insert n (fn + g n) o
            , path      = Map.insert n parent p
            , distances = Map.insert n fn d },
              t)
          (Nothing, True,  Just _, Just fp) | fn < fp   -> (aStar
            { opened = PSQ.insert n (fn + g n) o
            , closed = Set.delete n            c
            , path   = Map.insert n parent p
            , distances = Map.insert n fn d },
              t)
          (Just _,  _,     Just _, Just fp) | fn < fp   -> (aStar
            { opened = PSQ.adjust (\_ -> fn + g n) n o
            , path   = Map.insert n parent p
            , distances = Map.insert n fn d },
              t)
          _                                             -> (aStar, t)

backtrack paths goal = reverse $ goal:unfoldr (\x -> fmap (\x -> (x, x)) (Map.lookup x paths)) goal
