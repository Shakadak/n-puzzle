== Current Felt Pain Points ==

= Performance =

Mainly the hashing. I'll have to study that. And probably also have to find a better data structure to represente the puzzle state.  
Currently using:
```Haskell
type Coord = (Int, Int)
type Grid = Array Int Coord

instance (Hashable e, Ix i) => Hashable (Array i e) where
    hashWithSalt = foldl' hashWithSalt
```
= Code =
Mainly the function evaluating each neighbor. I think I should reduce the size of the record too.
```Haskell
eval :: (Num cost, Ord a, Ord cost, Hashable a) =>
    a -> (AStar a cost, (a, cost)) -> (AStar a cost, (a, cost))
eval neighbor skip@(aStar, t@(currentNode, currentCost)) =
    if Map.member neighbor (closed aStar)
       then skip
       else do let tmpCost = currentCost + cost aStar currentNode neighbor
               if maybe False (tmpCost >) (Map.lookup neighbor (distances aStar))
                  then skip
                  else (aStar
                          { opened = PSQ.insert neighbor (tmpCost + heuristic aStar neighbor) neighbor (opened aStar)
                          , path = Map.insert neighbor currentNode (path aStar)
                          , distances = Map.insert neighbor tmpCost (distances aStar) }
                           , t)
```
