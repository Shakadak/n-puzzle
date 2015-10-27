aStar goal start =
    runAStar (PSQ.singleton start (heuristic start) (Set.empty) (Map.singleton start Nothing)

runAStar open closed parents = do
    (current :-> _, open) <- PSQ.viewMin open
    Set.insert current closed
    if goal current
       then backtrack parents current
       else let (open, closed, parents) = foldl expand (open, closed, parents) (neighbors current)
             in runAStar open closed parents

expand (open, closed, parents) neighbor =
    case (PSQ.lookup neighbor open, Set.member neighbor closed) of
      (Nothing, False)  -> (PSQ.insert neighbor (cost neighbor), closed, Map.insert neighbor current)
      (_,       True) |  -> (open, closed)
      (_,       _)      -> (open, closed, parents)



-- create the open list of nodes, initially containing only our starting node
PSQ.singleton start (heuristic start)
-- create the closed list of nodes, initially empty
Set.empty
-- while (we have not reached our goal)
    -- consider the best node in the open list (the node with the lowest f value)
    (current :-> _, opened) <- PSQ.minView opened
    if goal current
       then we're done
       else
           -- move the current node to the closed list and consider all of its neighbors
            Set.insert current closed
           -- for (each neighbor)
                case (PSQ.lookup n opened, Map.lookup n closed) of
                -- if (this neighbor is in the closed list and our current g value is lower)
                (Nothing, Just (_, g)) | (c_g + (dist c n) < g)
                       -- update the neighbor with the new, lower, g value
                       -- change the neighbor's parent to our current node (c)
                    -> (opened, Map.adjust (\_ -> (c, c_g + dist c n)) n closed)
                -- else if (this neighbor is in the open list and our current g value is lower)
                (Just
                      then update the neighbor with the new, lower, g value
                           change the neighbor's parent to our current node
                      else this neighbor is not in either the open or closed list {
                          add the neighbor to the open list and set its g value
