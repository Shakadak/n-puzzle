module NPuzzle where
import Data.Array.Unboxed
import Data.List

data Direction = Up | Down | Left | Right

solution size = fst $ go' size
    where start n = ([], 0:[n ^ 2 - 1, n ^ 2 - 2..1])
          rotateClock = transpose . reverse
          update (s, d) n = (reverse (take n d) : rotateClock s, drop n d)
          go' n = foldl' update (start n) (fmap (`div` 2) [2..2*n])

cost one step = 1

expand state = map (swap state) [Up, Down, Left, Right] >>= maybe [] pure

swap state Up
