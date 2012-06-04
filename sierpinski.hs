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

drawDot :: GLfloat -> GLfloat -> IO ()
drawDot x y = do
  clear [ColorBuffer]
  color blue
  renderPrimitive Points $ vertex $ Vertex2 x y
  flush

reshape s@(Size w h) = do 
  viewport $= (Position 0 0, s)


display' angle position = do 
  clear [ColorBuffer]
  loadIdentity
  (x,y) <- get position
  translate $ Vector3 x y 0
  preservingMatrix $ do 
    a <- get angle
    rotate a $ Vector3 0 0 (1::GLfloat)
    scale 0.7 0.7 (0.7::GLfloat)
  swapBuffers

idle angle delta = do
  a <- get angle
  d <- get delta
  angle $=! (a+d) --parens needed for a bug in StateVar
  postRedisplay Nothing
