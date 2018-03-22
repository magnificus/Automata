import Graphics.UI.GLUT
import Automata 
import Data.Set
import Control.Concurrent
import Data.IORef

dist = 0.025
scaleS = 0.05
both f (x,y) = (f x, f y)
scalePoint = both (*scaleS)
getBox (x,y) = [(x-dist, y-dist, 0),(x + dist, y - dist, 0.0), (x+dist, y + dist, 0), (x-dist,y + dist, 0)]

lifeToPoints :: Int -> [(GLfloat, GLfloat, GLfloat)]
lifeToPoints n = concat $ toList $Data.Set.map (getBox . scalePoint . (both fromIntegral))  $ getGeneration n

main :: IO ()
main =  do
  (_progName, _args) <- getArgsAndInitialize
  i <- newIORef 0       -- new IORef i
  _window <- createWindow "LIFE"
  --displayCallback $= (display i)
  idleCallback $= Just (display i)
  mainLoop
 
display m = do 
  readIORef m >>= print -- print it
  clear [ColorBuffer]
  modifyIORef m (+1)    -- increase it by 1
  generation <- readIORef m
  renderPrimitive Quads  $
     mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) $ lifeToPoints generation
  threadDelay 500000
  flush


