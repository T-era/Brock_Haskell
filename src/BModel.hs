module BModel(windowWidth, windowHeight, iniChars, moveChars, boardLeft, boardRight, Charactors(..), genChars,
  Board(Board), Ball(Ball), Pos, Block(..), BlockType(..)) where

import Data.Maybe as M

import BBase
import BBlock

data Charactors = Charactors { ball :: Ball, board :: Board, blocks :: [Block], remains :: Int } deriving Show

iniBall = Ball (10,290) (2,2)
iniBoard = Board (windowWidth / 2) 20
iniBlocks = [Block (100,10) (8, 5) (LifeBlock 1)]
iniChars = genChars iniBall iniBoard iniBlocks

genChars ball board blocks = Charactors ball board blocks remainBlocks
  where
    remainBlocks = length blocksAbleCrush
    blocksAbleCrush = filter ableCrush blocks
    ableCrush (Block _ _ FixedBlock) = False
    ableCrush _                      = True

moveChars :: Double -> Charactors -> Charactors
moveChars rndT chars = reflectAll rndT chars{ ball = ballNext }
  where
    (Charactors (Ball (x,y) (dx,dy)) board blocks _) = chars
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
_reflectAll rndT done chars
  | blocks chars == [] = chars{ blocks = done }
  | otherwise          = _reflectAll rndT done' chars{ ball = ball', blocks = bs, remains = rem' }
  where
    (Charactors ball _ (block:bs) rem) = chars
    (done', rem')
      | block' == Nothing = (done, rem - 1)
      | otherwise         = (M.fromJust block':done, rem)
    (block', ball') = reflect rndT (block, ball)

stepOfMove = 3
boardLeft (Board x width)
  | x - width < 0 + stepOfMove = Board width width
  | otherwise                  = Board (x - stepOfMove) width
boardRight (Board x width)
  | x + width > windowWidth + stepOfMove = Board (windowWidth - width) width
  | otherwise                            = Board (x + stepOfMove) width
