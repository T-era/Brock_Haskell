module BStages(ApplicationMode(..), stepMode, stageBlocks, onPlay) where

import BBase
import BBlock

data ApplicationMode = Staging | Stage Int | Ending deriving (Show, Eq)

stepMode Staging = Stage 0
stepMode (Stage n)
  | n < length stageBlocks = Stage (n+1)
  | otherwise              = Ending

stageBlocks :: [[Block]]
stageBlocks = [
  [Block (200,1) (8,5) (LifeBlock 2)],
  [Block (x,y) (8, 5) (LifeBlock (if odd (floor (x / 20+y / 20)) then 1 else 2))
    | x <- [20, 40 .. windowWidth - 20]
    , y <- [20, 40 .. 100]]
  ]

onPlay :: ApplicationMode -> Bool
onPlay (Stage _) = True
onPlay _         = False
