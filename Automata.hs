module Automata where
import Data.Set
import qualified Data.Foldable as Fold
import Control.Monad

type Pos = (Int,Int)
type Grid = Set(Pos)

neighbors :: Grid -> Pos -> Int
neighbors a (b,c) = Fold.foldl (\z d -> (if (member d a) then (z+1) else z)) 0 (adjacent (b, c))

adjacent (b,c) = [(x,y) | x <- [b-1..b+1], y <-[c-1..c+1]]

alive :: Grid -> Pos -> Bool
alive a b 
 | member b a = aliveN < 5 && aliveN > 2
 | otherwise = aliveN == 3
 where
  aliveN = neighbors a b 

nextG :: Grid -> Grid
nextG a = Data.Set.filter (alive a) interesting
 where
  interesting = fromList $ concat $ Data.Set.map adjacent a
pentomo = [(2,1),(3,1),(2,2),(2,3), (1,2)]
getGeneration = (!!) (iterate nextG (fromList pentomo))
