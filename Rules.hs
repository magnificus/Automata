module Rules where

import Data.Set
import qualified Data.Foldable as Fold

type RuleS = [Pos]
type Rule = (Int, Int, Int)
type Pos = (Int,Int)
type Grid = Set(Pos)

type AliveFilter = Grid -> Pos -> Bool

conway = aliveNeighbors (3,2,3)
generous = aliveNeighbors (1,1,4)
mid1 = aliveNeighbors (3,1,3)
mid2 = aliveNeighbors (1,4,4)

aliveNeighbors :: Rule -> AliveFilter
aliveNeighbors (n1, n2, n3) a b 
 | member b a = aliveN <= n3 + 1 && aliveN > n2 
 | otherwise = aliveN == n1
 where
  aliveN = neighbors a b

crossMod = aliveSpecific [(1,0),(0,1),(-1,0),(0,-1)]
simpleMod = aliveSpecific [(1,0)]

-- an XOR for all sent tiles
aliveSpecific :: RuleS -> AliveFilter
aliveSpecific r a b = numActive `mod` 2 == 0 
 where
  numActive = length $ Prelude.filter (\d -> (member d a)) (relative b r)

relative :: Pos -> RuleS -> [Pos]
relative (b1,b2) p = Prelude.map (\(a1,a2) -> (a1+b1,a2+b2)) p

adjacent :: Pos -> [Pos]
adjacent (b,c) = [(x,y) | x <- [b-1..b+1], y <-[c-1..c+1]]

neighbors :: Grid -> Pos -> Int
neighbors a b = length $ Prelude.filter (\d -> (member d a)) (adjacent b)
