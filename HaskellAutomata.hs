import Data.Set
import qualified Data.Foldable as Fold

type Pos = (Int,Int)
type Grid = [Pos]

neighbors :: Grid -> Pos -> Int
neighbors a (b,c) = Fold.foldl (\z d -> (if (elem d a) then (z+1) else z)) 0 numbers
 where 
  numbers = [(x,y) | x <- [b-1..b+1], y <-[c-1..c+1]]

alive :: Grid -> Pos -> Bool
alive a b 
 | elem b a = aliveN < 4 && aliveN > 1
 | otherwise = aliveN == 3
 where
  aliveN = neighbors a b 

line a n y = concat $ Fold.foldl (++) (Prelude.map (toC. alive a) $ zip ( repeat y ) [0..n] ) [["\n"]]
gridString a n = Fold.foldl1 (++) $ (Prelude.map (line a n) [0..n])

toC a
 | a = "X"
 | otherwise = "O"

-- creates the new generation from the old one
tick :: Grid -> Int -> Grid
tick a n = Prelude.filter (alive a) [(x,y) | x <- [0..n], y <- [0..n]]
 
