import Graphics.UI.GLUT
import Automata 
import Data.Set
myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]


dist = 1  
getBox (x,y) = ((x-dist, y-dist, 0),(x + dist, y - dist, 0), (x+dist, y + dist, 0), (x-dist,y + dist, 0))
lifeToPoints = Data.Set.map getBox $ getGeneration 5

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
     mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
  flush
