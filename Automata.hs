module Automata where
import Rules
import Data.Set
import Control.Monad
import System.Random


--randomList :: Int a => a -> a ->  (a,g) -> IO [Int]
--randomList a b g = randomRs (a,b) g
--getStdGen >>= return . randomRs (a,b)a
--
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
--
--
--fromList [(x,y) | x<-[-50..50], y<-[-50..50]]
pentomo = [(2,1),(3,1),(2,2),(2,3), (1,2)]
rand = [(2,1),(3,1),(2,2),(2,3),(1,2),(3,3),(4,4),(4,5),(6,5)]
getGeneration a = (!!) (iterate nextG a)

--main = replicateM 10 (randomIO :: IO Integer) >>= print

