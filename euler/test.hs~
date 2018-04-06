import System.Environment
import Data.List.Split
type Point = (Int,Int)
sign (p1x,p1y) (p2x, p2y) (p3x, p3y) = (p1x - p3x) * (p2y - p3y) - (p2x - p3x) * (p1x - p3y)

toI l i = read (l!!i) :: Int
toP l n = (toI l n, toI l (n+1))

lineToPoints :: [Char] -> [Point]
lineToPoints l = [toP a 0, toP a 2, toP a 4]
 where a = splitOn "," l 

inTri :: Point -> Point -> Point -> Point -> Bool
inTri pt p1 p2 p3 = (v1 == v2) && (v2 == v3)
 where v1 = sign pt p1 p2 < 0
       v2 = sign pt p2 p3 < 0
       v3 =  sign pt p3 p1 < 0

fromFile f = do
 r <- readFile f
 let content = lines r
 let points = map lineToPoints content 
 let res = length $ filter (\a -> inTri (0,0) (a!!0) (a!!1) (a!!2)) points
 return res 
