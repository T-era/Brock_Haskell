import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Exit
import Data.IORef

import BBase
import BBlock

myWindowSize = Size (ceiling windowWidth) (ceiling windowHeight)
windowWidth :: Double
windowWidth = 300
windowHeight :: Double
windowHeight = 300
black = Color4 0 0 0 1
lColor = Color4 0.5 0 0 1
red = Color4 0.2 0 0 1
blue = Color4 0 0 1 1
dblue = Color4 0 0 0.5 1
iniBlocks = [Block (Pos (x,y)) (8, 5) (LifeBlock 2) | x <- [20, 40 .. windowWidth - 20], y <- [20,40 .. 100]]

data Ball = Ball { pos :: (Double, Double), dir :: (Double, Double) }
data Board = Board { bPos :: Double, width :: Double }

main :: IO ()
main = do
  getArgsAndInitialize
  ballRef <- newIORef (Ball (30,0) (2,2))
  boardRef <- newIORef (Board (windowWidth / 2) 10)
  blockRef <- newIORef iniBlocks

  initialWindowSize $= myWindowSize
  createWindow ""
  scale (1::GLfloat) 1 1
  clearColor $= black
  displayCallback $= showField ballRef boardRef blockRef
  addTimerCallback 1 (stepBall ballRef boardRef blockRef)
  keyboardCallback $= Just (keyboard boardRef)
  idleCallback $= Just idle
  mainLoop
idle :: IdleCallback
idle = postRedisplay Nothing

stepBall ballRef boardRef blockRef = do
  board <- readIORef boardRef
  modifyIORef ballRef (moveBall board)
  addTimerCallback 1 (stepBall ballRef boardRef blockRef)
  showField ballRef boardRef blockRef

moveBall board (Ball (x,y) (dx,dy)) = reflect ballNext board
  where
    ballNext = Ball (nx,ny) (ndx,ndy)
    tempX = x + dx
    tempY = y + dy
    (nx, ndx)
      | obX       = (x, 0 - dx)
      | otherwise = (tempX, dx)
    (ny, ndy)
      | obY       = (y, 0 - dy)
      | otherwise = (tempY, dy)
    obX = tempX < 0 || tempX > windowWidth
    obY = tempY < 0

reflect ball@(Ball (_,y) (_, dy)) board
  | y < 290 || 296 > y || dy < 0 = ball
  | otherwise                    = __reflect ball board
__reflect (Ball pos@(x, _) (dx, dy)) (Board bx width) = Ball pos nd
  where
    absDx = abs dx
    lEdge = bx - width
    rEdge = bx + width
    nd
      | x == lEdge && dx < 0 = (-absDx * 1.5, - dy)
      | x == lEdge && dx >= 0 = (-absDx, - dy * 1.5)
      | x == rEdge && dx > 0 = ( absDx * 1.5, - dy)
      | x == rEdge && dx <= 0 = ( absDx, - dy * 1.5)
      | x >= lEdge && x <= rEdge = (dx, - dy)
      | otherwise                = (dx, dy)


keyboard :: IORef Board -> KeyboardCallback
keyboard boardRef c _ = do
  case c of
    'a' -> modifyIORef boardRef moveLeft
    'd' -> modifyIORef boardRef moveRight
    _   -> return ()


moveLeft (Board x width)
  | x - width - 3 < 0 = Board width width
  | otherwise         = Board (x - 3) width
moveRight (Board x width)
  | x + width + 3 > windowWidth = Board (windowWidth - width) width
  | otherwise                   = Board (x + 3) width

myMouseMotionEvent _    = exitWith ExitSuccess

showField ballRef boardRef blockRef = do
  ball <- readIORef ballRef
  board <- readIORef boardRef
  blocks <- readIORef blockRef
  clear [ColorBuffer]
  drawBall ball
  drawBoard board
  drawBlocks blocks
  flush
  swapBuffers

drawBall (Ball (x, y) _) = do
  currentColor $= blue
  renderPrimitive Polygon $ do
    vertex $ conv $ Pos (x-1,y)
    vertex $ conv $ Pos (x,y-1)
    vertex $ conv $ Pos (x+1,y)
    vertex $ conv $ Pos (x,y+1)

drawBoard (Board x width) = do
  currentColor $= dblue
  renderPrimitive Polygon $ do
    vertex $ conv $ Pos (x-width,290)
    vertex $ conv $ Pos (x-width,295)
    vertex $ conv $ Pos (x+width,295)
    vertex $ conv $ Pos (x+width,290)

drawBlocks [] = return ()
drawBlocks (s:ls) = do
  drawBlock s
  drawBlocks ls
  -- TODO

drawBlock (Block (Pos (x,y)) (width, height) (LifeBlock n)) = do
  currentColor $= colorOf n
  renderPrimitive Polygon $ do
    vertex $ conv $ Pos (x-width,y-height)
    vertex $ conv $ Pos (x-width,y+height)
    vertex $ conv $ Pos (x+width,y+height)
    vertex $ conv $ Pos (x+width,y-height)

colorOf 2 = red
colorOf 1 = red
colorOf _ = blue

-- 論理座標を表示座標に変換します。
conv :: Pos Double -> Vertex2 GLfloat
conv (Pos (x, y)) = Vertex2 dispX dispY
  where
    dispX = realToFrac $ (x * 2.0 - windowWidth) / windowWidth
    dispY = realToFrac $ (windowHeight - y * 2.0) / windowHeight
