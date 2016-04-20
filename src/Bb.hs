import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Exit
import Data.IORef
import Data.Maybe as M
import Control.Monad
import System.Random

import BModel

myWindowSize = Size (ceiling windowWidth) (ceiling windowHeight)

black = Color4 0 0 0 1
colorWeek = Color4 0.5 0.3 0.2 1
colorFixed = Color4 1 0.8 0.5 1
colorLL = Color4 0.5 0.3 0.3 1
colorL2 = Color4 0.4 0.2 0.2 1
colorL1 = Color4 0.3 0.1 0.1 1
colorL0 = Color4 0.2 0 0 1
colorBall = Color4 0.5 0.5 1 1
colorBoard = Color4 0 0 1 1

main :: IO ()
main = do
  getArgsAndInitialize
  charRef <- newIORef iniChars

  initialWindowSize $= myWindowSize
  createWindow ""
  scale (1::GLfloat) 1 1
  clearColor $= black
  displayCallback $= showField charRef
  addTimerCallback 1 (stepBall charRef)
  keyboardCallback $= Just (keyboard charRef)
  idleCallback $= Just idle
  mainLoop

idle :: IdleCallback
idle = postRedisplay Nothing

stepBall charRef = do
  (Charactors _ board _) <- readIORef charRef
  rndT <- randomRIO (0, pi)
  modifyIORef charRef (moveChars rndT)
  addTimerCallback 0 (stepBall charRef)
  showField charRef

keyboard :: IORef Charactors -> KeyboardCallback
keyboard charRef c _ = do
  case c of
    'a' -> modifyIORef charRef $ moveBoard boardLeft
    'd' -> modifyIORef charRef $ moveBoard boardRight
    _   -> return ()

moveBoard fBoard chars@(Charactors _ _board _) = chars { board = fBoard _board }

showField charRef = do
  (Charactors ball board blocks) <- readIORef charRef
  clear [ColorBuffer]
  drawBall ball
  drawBoard board
  drawBlocks blocks
  flush
  swapBuffers

drawBall (Ball (x, y) _) = do
  currentColor $= colorBall
  renderPrimitive Polygon $ do
    convVert $ Pos (x-2,y)
    convVert $ Pos (x,y-2)
    convVert $ Pos (x+2,y)
    convVert $ Pos (x,y+2)

drawBoard (Board x width) = do
  currentColor $= colorBoard
  drawRectangle (x, 293) (width, 3)

drawBlocks blocks = foldM drawBlockChain () blocks
  where
    drawBlockChain _ = drawBlock

drawBlock (Block (Pos (x,y)) (width, height) bType) = do
  currentColor $= colorOfType bType
  drawRectangle (x, y) (width, height)

drawRectangle (x, y) (width, height) = renderPrimitive Polygon $ do
  convVert $ Pos (x-width,y-height)
  convVert $ Pos (x-width,y+height)
  convVert $ Pos (x+width,y+height)
  convVert $ Pos (x+width,y-height)

colorOfType WeekBlock = colorWeek
colorOfType FixedBlock = colorFixed
colorOfType (LifeBlock n) = colorOfLife n
colorOfLife 0 = colorL0
colorOfLife 1 = colorL1
colorOfLife 2 = colorL2
colorOfLife _ = colorLL

-- 論理座標を表示座標に変換します。
conv :: Pos Double -> Vertex2 GLfloat
conv (Pos (x, y)) = Vertex2 dispX dispY
  where
    dispX = realToFrac $ (x * 2 - windowWidth) / windowWidth
    dispY = realToFrac $ (windowHeight - y * 2) / windowHeight
convVert = vertex.conv
