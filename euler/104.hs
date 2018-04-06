import Data.List
fibs = unfoldr (\(a,b) -> Just (a,(b,a+b))) (0,1)

indInts :: Integer -> [Integer]
indInts 0 = []
indInts n = indInts (n `div` 10) ++  [(n `mod` 10)]

hasFirst = containsNums . (take 9) . indInts
hasLast = containsNums . indInts . (\a -> a `mod` 1000000000)
containsNums a = [1..9] == sort a

satBoth f1 f2 b = f1 b && f2 b

containsAll :: [Integer] -> [Integer] -> Bool
containsAll (x:xs) ys = elem x ys && containsAll xs ys
containsAll [] ys = True

main = do putStr $ show $ findIndex (satBoth hasFirst hasLast) fibs
