aStar goal start =
   runAStar (PSQ.singleton start (heuristic start) (Set.empty) (Map.singleton start Nothing)

runAStar open closed = do
    (current :-> _, open) <- PSQ.viewMin open
    Set.insert current closed
    ns <- neighbors current
    (open, closed) <- foldl expand (open, closed) ns

expand (open, closed) neighbor =
    case (PSQ.lookup neighbor open, Set.member neighbor closed) of
      (Nothing, False)  -> (PSQ.insert neighbor (cost neighbor), closed)
      (_,       True)   -> (open, closed)
