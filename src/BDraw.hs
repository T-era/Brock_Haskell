module BDraw(showField) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Control.Monad

import BModel
import BStages

myWindowSize = Size (ceiling windowWidth) (ceiling windowHeight)

colorWeek = Color4 0.5 0.3 0.2 1
colorFixed = Color4 1 0.8 0.5 1
colorLL = Color4 1.0 0.4 0.4 1
colorL2 = Color4 0.8 0.3 0.3 1
colorL1 = Color4 0.6 0.2 0.2 1
colorL0 = Color4 0.4 0.1 0.1 1
colorBall = Color4 0.5 0.5 1 1
colorBoard = Color4 0 0 1 1

showField stageRef charRef = do
  stage <- readIORef stageRef
  clear [ColorBuffer]
  if onPlay stage then do
    drawPlayingField charRef
  else if stage == Ending then do
    drawPlayingField charRef
    currentColor $= colorBall
    renderStringCenter 0 "Failure..."
  else if stage == Completed then do
    currentColor $= colorBall
    renderStringsCenter ["Complete!", "... so what!?"]
  else do
    currentColor $= colorBall
    renderStringCenter 0 "Push any key!"
  flush
  swapBuffers

renderStringCenter y str = preservingMatrix $ do
  scale (0.001::GLfloat) 0.001 0.001
  w <- stringWidth Roman str
  h <- fontHeight Roman
  translate (Vector3 (-0.5*(fromIntegral w)) (y*h*2) 0 ::Vector3 GLfloat)
  renderString Roman str

renderStringsCenter lst = let
  dy = (fromIntegral $ length lst - 1) * (-0.5)
  in do
    mapM_ (\ (y, str) -> renderStringCenter (fromIntegral y * dy)  str)
      [(i, lst !! i) | i <- [0..length lst - 1]]

drawPlayingField charRef = do
  (Charactors ball board blocks _) <- readIORef charRef
  drawBall ball
  drawBoard board
  drawBlocks blocks

drawBall (Ball (x, y) _) = do
  currentColor $= colorBall
  renderPrimitive Polygon $ do
    convVert (x-2,y)
    convVert (x,y-2)
    convVert (x+2,y)
    convVert (x,y+2)

drawBoard (Board x width) = do
  currentColor $= colorBoard
  drawRectangle ((x, 293), (width, 3))

drawBlocks blocks = foldM drawBlockChain () blocks
  where
    drawBlockChain _ = drawBlock

drawBlock (Block ((x,y)) (width, height) bType) = do
  currentColor $= colorOfType bType
  drawRectangle ((x, y), (width, height))

drawRectangle ((x, y), (width, height)) = renderPrimitive Polygon $ do
  convVert (x-width,y-height)
  convVert (x-width,y+height)
  convVert (x+width,y+height)
  convVert (x+width,y-height)

colorOfType WeekBlock = colorWeek
colorOfType FixedBlock = colorFixed
colorOfType (LifeBlock n) = colorOfLife n
colorOfLife 0 = colorL0
colorOfLife 1 = colorL1
colorOfLife 2 = colorL2
colorOfLife _ = colorLL

-- 論理座標を表示座標に変換します。
conv :: Pos -> Vertex2 GLfloat
conv (x, y) = Vertex2 dispX dispY
  where
    dispX = realToFrac $ (x * 2 - windowWidth) / windowWidth
    dispY = realToFrac $ (windowHeight - y * 2) / windowHeight
convVert = vertex.conv
