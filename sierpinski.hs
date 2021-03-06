import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Random
import Data.IORef

blue = (Color3 0 0 (1.0::GLfloat))

main = do
  (progname,_) <- getArgsAndInitialize
  createWindow "Hello World"
  reshapeCallback $= Just reshape

  angle <- newIORef (0.0::GLfloat)
  delta <- newIORef (1.0::GLfloat)
  --position <- newIORef (0.0::GLfloat, 0.0)
  --keyboardMouseCallback $= Just (keyboardMouse delta position)
  idleCallback $= Just (idle angle delta)
  displayCallback $= (chaosGame angle)
  mainLoop


reshape s@(Size w h) = do 
  viewport $= (Position 0 0, s)



idle angle delta = do
  a <- get angle
  d <- get delta
  angle $=! (a+d) --parens needed for a bug in StateVar
  postRedisplay Nothing



data Point2d = Point2d { 
               xcoord :: GLfloat,
               ycoord :: GLfloat 
               } deriving (Show)


chaosGame ::  HasGetter g => g Float -> IO ()
chaosGame angle = do
  clear [ColorBuffer]

  a <- get angle

  let scale = 0.75
  let testPt0 = unitCirclePoint scale (0.0 + a)
  let testPt1 = unitCirclePoint scale (120.0 + a)
  let testPt2 = unitCirclePoint scale (240.0 + a)

  seed <- newStdGen
  let rndList = take 10000 $ randoms seed

  drawDot testPt0
  drawDot testPt1
  drawDot testPt2

  chaosStep testPt0 testPt0 testPt1 testPt2 rndList

  flush

{--
A step for a chaos game, given three points and a list of random numbers
we will draw at the midpoint of two of the random points selected
randomly
--}
chaosStep :: Point2d -> Point2d -> Point2d -> Point2d -> [Int] -> IO ()
chaosStep _ _ _ _ [] = return ()
chaosStep startPt pt0 pt1 pt2 (n:ns) = do
  --Randomly choose a second point
  let rndPt = case n `mod` 3 of
                0 -> pt0
                1 -> pt1
                2 -> pt2
  
  --Move current position to halfway between the starting point and our random point
  let mdPt = midpoint startPt rndPt
  drawDot mdPt
  chaosStep mdPt pt0 pt1 pt2 ns

--Given two points, find the mid point between them
midpoint :: Point2d -> Point2d -> Point2d
midpoint (Point2d x1 y1) (Point2d x2 y2) = Point2d ((x1 + x2) / 2) ((y1 + y2) / 2)

drawDot :: Point2d -> IO ()
drawDot (Point2d x y) = do
  color blue
  renderPrimitive Points $ vertex $ Vertex2 x y

--Convert radians to degrees
fromDegrees :: Float -> Float
fromDegrees degrees = degrees * pi / 180

--Create a point2d that is properly scaled positioned on a specified degree on a unitCircle
unitCirclePoint :: Float -> Float -> Point2d
unitCirclePoint scale degrees = Point2d ((*scale) . cos . fromDegrees $ degrees) ((*scale) . sin . fromDegrees $ degrees)
