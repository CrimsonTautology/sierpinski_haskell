import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Bindings
import Data.IORef

main = do
  (progname,_) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  createWindow "Hello World"
  reshapeCallback $= Just reshape
  angle <- newIORef (0.0::GLfloat)
  delta <- newIORef (0.1::GLfloat)
  position <- newIORef (0.0::GLfloat, 0.0)
  keyboardMouseCallback $= Just (keyboardMouse delta position)
  idleCallback $= Just (idle angle delta)
  displayCallback $= (display angle position)
  mainLoop

