module Automata where
import Rules
import Data.Set
import Control.Monad
import System.Random

randomList :: Int -> IO([Int])
randomList n = replicateM n $ randomRIO (-50,50)
numTake = 10000

just1 :: IO [Pos]
just1 = return [(0,0)] 

randomPositions = do
 g <- getStdGen
 seq1 <- randomList numTake
 seq2 <- randomList numTake
 return $ zip seq1 seq2

nextG :: AliveFilter -> Grid -> Grid
nextG f a = Data.Set.filter (f a) interesting
 where
  interesting = fromList $ concat $ Data.Set.map adjacent a

pentomo = [(2,1),(3,1),(2,2),(2,3), (1,2)]
--getGeneration :: Grid -> AliveFilter -> Int -> Grid
getGeneration a f= (!!) (iterate (nextG f) a)

