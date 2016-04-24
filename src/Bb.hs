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
  char <- readIORef charRef
  stage <- readIORef stageRef
  if cleared char then do
    modifyIORef stageRef stepMode
    modifyIORef charRef $ charAtStage stage
    addTimerCallback 20 (stepBall stageRef charRef)
  else if died char then do
    modifyIORef stageRef (\_ -> Ending)
  else do
    addTimerCallback 20 (stepBall stageRef charRef)
  showField stageRef charRef

cleared (Charactors _ _ _ n) = n == 0
died char = y > windowHeight
  where
    (Ball (_, y) _) = ball char

charAtStage (Stage n) (Charactors ball board _ _) = genChars ball board (stageBlocks !! n)
charAtStage _ c = c

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
