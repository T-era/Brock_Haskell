module BBlock(Block(..), BlockType(..), crush) where

import Data.Maybe
import BBase

data Block = Block { pos :: (Pos Double), size :: (Double, Double), blType:: BlockType } deriving Eq

data BlockType = WeekBlock
    | FixedBlock
    | LifeBlock Int
    deriving Eq

crush :: Pos Double -> Block -> Maybe Block
crush p block@(Block pos size bltype)
  | hit p pos size = nextBlock
  | otherwise      = Just block
  where
    nextBlock
      | nextBlockType == Nothing = Nothing
      | otherwise                = Just $ block { blType = fromJust nextBlockType }
    nextBlockType = crushType bltype

hit (Pos (bx, by)) (Pos (x, y)) (width, height) = xHit && yHit
  where
    xHit = _hit bx x width
    yHit = _hit by y height
    _hit a b s = a >= b - s && a <= b + s


crushType WeekBlock = Nothing
crushType FixedBlock = Just FixedBlock
crushType (LifeBlock 0) = Nothing
crushType (LifeBlock n) = Just (LifeBlock (n-1))

instance Reflector Block where
  rect (Block (Pos (x, y)) (width, height) _) = (Pos (x, y), (width, height))
  cols (Block _ _ WeekBlock) = Nothing
  cols b@(Block _ _ FixedBlock) = Just b
  cols b@(Block _ _ (LifeBlock n))
    | n == 0    = Nothing
    | otherwise = Just b{ blType = LifeBlock (n - 1) }
