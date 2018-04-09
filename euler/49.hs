import Data.List
import Data.Maybe
import Data.Set
isPrime n = isNothing $ find (\x -> ((n `mod` x) == 0)) [2..(floor ( sqrt (fromIntegral n)))]

primes= fromList $ Prelude.filter isPrime [1..10000] 

sameNumbers a = (==) (digitsS a). digitsS 
digitsS :: Integer -> [Char]
digitsS = sort . show

testSeq (s,n) = (== 3) $ length $ Prelude.filter (\x -> (member x primes) && sameNumbers s x) [s,n + s,n*2 + s]

getSeq = Prelude.filter testSeq [(x,y) | x <- [1000..3000], y <- [1..3333]] 
 
