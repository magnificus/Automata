module Automata where
import Rules
import Data.Set
import Control.Monad
import System.Random

randomList :: Int -> IO([Int])
randomList n = replicateM n $ randomRIO (-50,50)

numTake = 1000
randomPositions = do
 g <- getStdGen
 seq1 <- randomList numTake
 seq2 <- randomList numTake
 return $ zip seq1 seq2

nextG :: Grid -> Grid
nextG a = Data.Set.filter (conway a) interesting
 where
  interesting = fromList $ concat $ Data.Set.map adjacent a

pentomo = [(2,1),(3,1),(2,2),(2,3), (1,2)]
getGeneration a = (!!) (iterate nextG a)

