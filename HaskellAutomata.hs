import Data.Set
import qualified Data.Foldable as Fold

type Pos = (Int,Int)
type Grid = Set(Pos)

neighbors :: Grid -> Pos -> Int
neighbors a (b,c) = Fold.foldl (\z d -> (if (elem d a) then (z+1) else z)) 0 (adjacent (b, c))

adjacent (b,c) = [(x,y) | x <- [b-1..b+1], y <-[c-1..c+1]]

alive :: Grid -> Pos -> Bool
alive a b 
 | elem b a = aliveN < 5 && aliveN > 2
 | otherwise = aliveN == 3
 where
  aliveN = neighbors a b 

line :: Grid -> Int -> Int -> [Char]
line a n y = concat $ Fold.foldl (++) (Prelude.map (toC. (flip member a)) $ zip ( repeat y ) [0..n] ) [["\n"]]
gridString a n = Fold.foldl1 (++) $ (Prelude.map (line a n) [0..n])

toC a
 | a = "X"
 | otherwise = "O"

nextG :: Grid -> Grid
nextG a = Data.Set.filter (alive a) interesting
 where
  interesting = fromList $ concat $ Data.Set.map adjacent a
 
