module BBlock(Block(..), BlockType(..), crush) where

import Data.Maybe
import BBase

data Block = Block { pos :: Pos, size :: (Double, Double), blType:: BlockType } deriving (Show, Eq)

data BlockType = WeekBlock
    | FixedBlock
    | LifeBlock Int
    deriving (Show, Eq)

crush :: Pos -> Block -> Maybe Block
crush p block@(Block pos size bltype)
  | hit p pos size = nextBlock
  | otherwise      = Just block
  where
    nextBlock
      | nextBlockType == Nothing = Nothing
      | otherwise                = Just block{ blType = fromJust nextBlockType }
    nextBlockType = crushType bltype

hit :: Pos -> Pos -> (Double, Double) -> Bool
hit (bx, by) (x, y) (width, height) = xHit && yHit
  where
    xHit = _hit bx x width
    yHit = _hit by y height
    _hit a b s = a >= b - s && a <= b + s


crushType WeekBlock = Nothing
crushType FixedBlock = Just FixedBlock
crushType (LifeBlock 0) = Nothing
crushType (LifeBlock n) = Just (LifeBlock (n-1))

instance Reflector Block where
  rect (Block (x, y) (width, height) _) = ((x, y), (width, height))
  cols (Block _ _ WeekBlock) = Nothing
  cols b@(Block _ _ FixedBlock) = Just b
  cols b@(Block _ _ (LifeBlock n))
    | n == 0    = Nothing
    | otherwise = Just b{ blType = LifeBlock (n - 1) }
