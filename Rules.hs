module Rules where

import Data.Set
import qualified Data.Foldable as Fold

type Rule = (Int, Int, Int)
-- how many cells are alive for that state to be active, lowest, max to turn on
type GRule = [Rule]
type Pos = (Int,Int)
type Grid = Set(Pos)
type AliveFilter = Grid -> Pos -> Bool

conway = aliveNeighbors (3,2,3)

aliveNeighbors :: Rule -> AliveFilter
aliveNeighbors (n1, n2, n3) a b 
 | member b a = aliveN <= n3 + 1 && aliveN > n2 
 | otherwise = aliveN == n1
 where
  aliveN = neighbors a b

adjacent :: Pos -> [Pos]
adjacent (b,c) = [(x,y) | x <- [b-1..b+1], y <-[c-1..c+1]]

neighbors :: Grid -> Pos -> Int
neighbors a b = length $ Prelude.filter (\d -> (member d a)) (adjacent b)
