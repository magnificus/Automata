import Graphics.UI.GLUT
import Automata 
import Data.Set
myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]

dist = 0.1
scaleS = 0.05
both f (x,y) = (f x, f y)
scalePoint = both (*scaleS)
getBox (x,y) = [(x-dist, y-dist, 0),(x + dist, y - dist, 0.0), (x+dist, y + dist, 0), (x-dist,y + dist, 0)]

lifeToPoints :: [(GLfloat, GLfloat, GLfloat)]
lifeToPoints = concat $ toList $Data.Set.map (getBox . scalePoint . (both fromIntegral))  $ getGeneration 6



main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  mainLoop
 
display :: DisplayCallback
display = do 
  clear [ColorBuffer]
  renderPrimitive Quads  $
     mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) lifeToPoints
  flush
