import Data.PSQueue as PSQ
import Data.Map     as Map
import Data.Set     as Set

aStarSearch goal start =
    runAStar PSQ.singleton start (heuristic start) (Set.empty) (Map.singleton start Nothing)

runAStar :: AStar a cost -> [] a
runAStar aStar = do
    (current :-> _, o) <- PSQ.minView (opened aStar)
    if (goal aStar) current
       then backtrack (path aStar) current
       else let (aStar, _) = Set.fold eval (aStar {opened = o, closed = Set.insert current (closed aStar)}, current) ((expand aStar) current)
             in runAStar aStar

data AStar a cost = AStar {
    expand    :: a -> Set a,
    goal      :: a -> Bool,
    opened    :: PSQ.PSQ a cost,
    closed    :: Set.Set a,
    distance  :: Map.Map a cost,
    path      :: Map.Map a (Maybe a),
    heuristic :: a -> cost,
    cost      :: a -> a -> cost }

eval :: a -> (AStar a cost, (a, cost)) -> (AStar a cost, (a, cost))
eval n (aStar, (parent, g)) =
    let gn = g + (cost aStar) parent n
        o = opened    aStar
        c = closed    aStar
        p = path      aStar
        h = heuristic aStar in
        case (PSQ.lookup n o, Set.member n c, Map.lookup n p) of
          (Nothing, False, _)                       -> (aStar {
              opened = PSQ.insert n (gn + h n) o,
              path   = Map.insert n (parent, gn) p },
              (parent, gn))
          (Nothing, True,  Just(_, gp)) | gn < gp   -> (aStar {
              opened = PSQ.insert n (gn + h n) o,
              closed = Set.delete n            c,
              path   = Map.insert n (parent, gn) p },
              (parent, gn))
          (Just(_), _,     Just(_, gp)) | gn < gp   -> (aStar {
              opened = PSQ.adjust n (gn + h n) o,
              path   = Map.insert n (parent, gn) p },
              (parent, gn))
          _                                         -> (aStar, (parent, gn))

backtrack = undefined
