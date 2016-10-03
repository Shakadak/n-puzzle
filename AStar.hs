module AStar (aStarSearch) where
import Data.HashPSQ as PSQ
import Data.Hashable
import Data.HashMap     as Map
import Data.Set     as Set
import Data.List    (unfoldr)
import Data.Maybe   (fromMaybe)

data AStar a cost = AStar {
    opened    :: PSQ.HashPSQ a cost a,
    closed    :: Map.Map a (),
    distances :: Map.Map a cost,
    path      :: Map.Map a a }

aStarSearch :: (Num cost, Ord a, Ord cost, Hashable a) =>
     (a -> Set a)       -- expand
  -> (a -> a -> cost)   -- cost
  -> (a -> cost)        -- heuristic
  -> (a -> Bool)        -- goal
  -> a                  -- start
  -> Maybe [a]          -- Maybe Path
aStarSearch expand cost heuristic goal start = runAStar expand cost heuristic goal AStar
    { opened    = PSQ.singleton start (heuristic start) start
    , closed    = Map.empty
    , path      = Map.empty
    , distances = Map.empty }

backtrack :: (Ord a, Hashable a) =>
    Map a a -> a -> [a]
backtrack paths end = reverse $ end:unfoldr (\s -> fmap (\x -> (x, x)) (Map.lookup s paths)) end

runAStar :: (Num cost, Ord a, Ord cost, Hashable a) =>
     (a -> Set a)       -- expand
  -> (a -> a -> cost)   -- cost
  -> (a -> cost)        -- heuristic
  -> (a -> Bool)        -- goal
  -> AStar a cost -> Maybe [a]
runAStar expand cost heuristic goal aStar = do
    (_, _, current, o) <- PSQ.minView (opened aStar)
    if goal current
       then return $ backtrack (path aStar) current
       else let aStar' = aStar { opened = o, closed = Map.insert current () (closed aStar) }
                currentCost = fromMaybe 0 (Map.lookup current $ distances aStar')
             in runAStar expand cost heuristic goal (Set.foldr (eval cost heuristic current currentCost) aStar' (expand current))

eval :: (Num cost, Ord a, Ord cost, Hashable a) =>
    (a -> a -> cost)    -- cost
    -> (a -> cost)      -- heuristic
    -> a                -- current node
    -> cost             -- cost from start to current node
    -> a                -- neighbor of current
    -> AStar a cost -> AStar a cost
eval cost heuristic currentNode currentCost neighbor aStar =
    if Map.member neighbor (closed aStar)
       then aStar
       else do let tmpCost = currentCost + cost currentNode neighbor
               if maybe False (tmpCost >) (Map.lookup neighbor (distances aStar))
                  then aStar
                  else aStar
                          { opened = PSQ.insert neighbor (tmpCost + heuristic neighbor) neighbor (opened aStar)
                          , path = Map.insert neighbor currentNode (path aStar)
                          , distances = Map.insert neighbor tmpCost (distances aStar) }
