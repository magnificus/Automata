import Graphics.UI.GLUT
import Automata 
import Rules
import Data.Set
import Control.Concurrent
import Data.IORef
import System.Random

dist = 0.0045
scaleS = 0.01
both f (x,y) = (f x, f y)
scalePoint = both (*scaleS)
getBox (x,y) = [(x-dist, y-dist, 0),(x + dist, y - dist, 0.0), (x+dist, y + dist, 0), (x-dist,y + dist, 0)]

lifeToPoints :: Set(Pos) -> [(GLfloat, GLfloat, GLfloat)]
lifeToPoints seq = concat $ toList $Data.Set.map (getBox . scalePoint . (both fromIntegral)) seq

main :: IO ()
main =  do
  (_progName, _args) <- getArgsAndInitialize
  i <- newIORef 0       -- new IORef i
  test <- newIORef [(1,2)]
  _window <- createWindow "LIFE"
  seq <- randomPositions
  s <- newIORef $ fromList seq
  reshapeCallback $= Just reshape
  displayCallback $= display i s
  idleCallback $= Just (display i s)
  mainLoop
 
display m s = do 
  readIORef m >>= print -- print it
  clear [ColorBuffer]
  l <- readIORef s
  renderPrimitive Quads  $
     mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) $ lifeToPoints l 
  modifyIORef m (+1)    -- increase it by 1
  modifyIORef s nextG
  threadDelay 30000
  flush

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

