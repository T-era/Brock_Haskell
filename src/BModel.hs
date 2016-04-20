module BModel(windowWidth, windowHeight, iniChars, moveChars, boardLeft, boardRight, Charactors(..),
  Board(Board), Ball(Ball), Pos(..), Block(..), BlockType(..)) where

import Data.Maybe as M

import BBase
import BBlock

(windowWidth, windowHeight) = (300, 300) :: (Double, Double)
iniBall = Ball (30,0) (2,2)
iniBoard = Board (windowWidth / 2) 20
iniBlocks = [
  Block (Pos (x,y)) (8, 5) (LifeBlock (if odd (floor (x / 20+y / 20)) then 1 else 2))
  | x <- [20, 40 .. windowWidth - 20]
  , y <- [20, 40 .. 100]]

data Charactors = Charactors { ball :: Ball, board :: Board, blocks :: [Block] }

iniChars = Charactors iniBall iniBoard iniBlocks

moveChars :: Double -> Charactors -> Charactors
moveChars rndT chars@(Charactors (Ball (x,y) (dx,dy)) board blocks) = reflectAll rndT chars{ ball = ballNext }
  where
    (_, ballNext) = reflect rndT (board, Ball (nx,ny) (ndx,ndy))
    tempX = x + dx
    tempY = y + dy
    (nx, ndx)
      | obX       = (x, - dx)
      | otherwise = (tempX, dx)
    (ny, ndy)
      | obY       = (y, - dy)
      | otherwise = (tempY, dy)
    obX = tempX < 0 || tempX > windowWidth
    obY = tempY < 0

reflectAll :: Double -> Charactors -> Charactors
reflectAll rndT chars = _reflectAll rndT [] chars
_reflectAll :: Double -> [Block] -> Charactors -> Charactors
_reflectAll _ done chars@(Charactors ball _ []) = chars{ blocks = done }
_reflectAll rndT done chars@(Charactors ball _ (block:bs)) = _reflectAll rndT done' chars{ ball = ball', blocks = bs }
  where
    done'
      | block' == Nothing = done
      | otherwise         = (M.fromJust block':done)
    (block', ball') = reflect rndT (block, ball)

boardLeft (Board x width)
  | x - width - 3 < 0 = Board width width
  | otherwise         = Board (x - 3) width
boardRight (Board x width)
  | x + width + 3 > windowWidth = Board (windowWidth - width) width
  | otherwise                   = Board (x + 3) width
