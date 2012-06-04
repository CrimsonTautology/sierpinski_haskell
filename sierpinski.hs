import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

blue = (Color3 0 0 (1.0::GLfloat))

main = do
  (progname,_) <- getArgsAndInitialize
  createWindow "Hello World"
  reshapeCallback $= Just reshape

  --angle <- newIORef (0.0::GLfloat)
  --delta <- newIORef (0.1::GLfloat)
  --position <- newIORef (0.0::GLfloat, 0.0)
  --keyboardMouseCallback $= Just (keyboardMouse delta position)
  --idleCallback $= Just (idle angle delta)
  displayCallback $= (drawDot 0 0)
  mainLoop


reshape s@(Size w h) = do 
  viewport $= (Position 0 0, s)



idle angle delta = do
  a <- get angle
  d <- get delta
  angle $=! (a+d) --parens needed for a bug in StateVar
  postRedisplay Nothing



{--
A step for a chaos game, given three points and a list of random numbers
we will draw at the midpoint of two of the random points selected
randomly
--}
data Point2d = Point2d { 
               xcoord :: GLfloat,
               ycoord :: GLfloat 
               } deriving (Show)


chaosStep :: Point2d -> Point2d -> Point2d -> Point2d -> [Int] -> IO ()
chaosStep _ _ _ [] = return ()
chaosStep startPt pt0 pt1 pt2 (n:ns) = do
  --Randomly choose a second point
  let rndPt = case n `mod` 3
      0 -> pt0
      1 -> pt1
      2 -> pt2

  --Move current position to halfway between the starting point and our random point
  mdPt = midpoint startPt rndPt
  drawDot mdPt

  putStrLn $ (show startPt) ++ "  " ++ (show rndPoint) ++ "  " ++ (show mdPt) --TODO Debug

--Given two points, find the mid point between them
midpoint :: Point2d -> Point2d -> Point2d
midpoint (Point2d x1 y1) (Point2d x2 y2) = Point2d ((x1 + x2) / 2) ((y1 + y2) / 2)

drawDot :: Point2d -> IO ()
drawDot (Point2d x y) = do
  clear [ColorBuffer]
  color blue
  renderPrimitive Points $ vertex $ Vertex2 x y
  flush

