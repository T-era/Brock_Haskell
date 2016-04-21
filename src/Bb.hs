import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Exit
import Data.IORef
import Data.Maybe as M
import Control.Monad
import System.Random

import BModel
import BStages
import BDraw

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
  stageRef <- newIORef Staging

  initialWindowSize $= myWindowSize
  createWindow ""
  scale (1::GLfloat) 1 1
  clearColor $= black
  --displayCallback $= showField stageRef charRef
  keyboardCallback $= Just (keyboard stageRef charRef)
  idleCallback $= Just idle
  showField stageRef charRef
  mainLoop

idle :: IdleCallback
idle = postRedisplay Nothing

stepBall stageRef charRef = do
  rndT <- randomRIO (0, pi)
  modifyIORef charRef (moveChars rndT)
  showField stageRef charRef
  char <- readIORef charRef
  stage <- readIORef stageRef
  putStrLn $ show $ remains char
  if cleared char then do
    putStrLn "CCC"
    modifyIORef charRef $ nextStage stage
    modifyIORef stageRef stepMode
  else if died char then do
    modifyIORef stageRef (\_ -> Ending)
  else do
    addTimerCallback 20 (stepBall stageRef charRef)

cleared (Charactors _ _ _ n) = n == 0
died char = y > windowHeight
  where
    (Ball (_, y) _) = ball char

nextStage (Stage n) (Charactors ball board _ _) = genChars ball board (stageBlocks !! n)

keyboard :: IORef ApplicationMode -> IORef Charactors -> KeyboardCallback
keyboard stageRef charRef c _ = do
  stage <- readIORef stageRef
  if onPlay stage then
    case c of
      'a' -> modifyIORef charRef $ moveBoard boardLeft
      'd' -> modifyIORef charRef $ moveBoard boardRight
      _   -> return ()
  else do
    modifyIORef stageRef (\_ -> Stage 0)
    addTimerCallback 1 (stepBall stageRef charRef)
  showField stageRef charRef

moveBoard fBoard chars = chars { board = fBoard $ board chars }
