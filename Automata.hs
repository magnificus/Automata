module Automata where
import Data.Set
import qualified Data.Foldable as Fold
import Control.Monad

type Pos = (Int,Int)
type Grid = Set(Pos)
type Rule = (Int, Int, Int)
neighbors :: Grid -> Pos -> Int
neighbors a (b,c) = Fold.foldl (\z d -> (if (member d a) then (z+1) else z)) 0 (adjacent (b, c))

adjacent (b,c) = [(x,y) | x <- [b-1..b+1], y <-[c-1..c+1]]
--adjacent (b,c) = Fold.foldl (\x y -> (((b-1, c-1), (b-1, c), (b-1, c+1), (b, c-1), (b, c+1), (b+1, c-1),(b+1, c), (b+1, c+1))
conway = (3,2,3)
generous = (1,1,4)
mid1 = (3,1,3)
mid2 = (1,4,4)

alive :: Rule -> Grid -> Pos -> Bool
alive (n1, n2, n3) a b 
 | member b a = aliveN <= n3 + 1 && aliveN > n2 
 | otherwise = aliveN == n1
 where
  aliveN = neighbors a b 

nextG :: Grid -> Grid
nextG a = Data.Set.filter (alive conway a) interesting
 where
  interesting = fromList $ concat $ Data.Set.map adjacent a
pentomo = [(2,1),(3,1),(2,2),(2,3), (1,2)]
getGeneration = (!!) (iterate nextG (fromList pentomo))
