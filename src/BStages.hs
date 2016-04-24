module BStages(ApplicationMode(..), stepMode, stageBlocks, onPlay) where

import BBase
import BBlock

data ApplicationMode = Staging | Stage Int | Ending | Completed deriving (Show, Eq)

stepMode Staging = Stage 0
stepMode (Stage n)
  | n < length stageBlocks = Stage (n+1)
  | otherwise              = Completed
stepMode s = s

stageBlocks :: [[Block]]
stageBlocks = [
  [Block (110,80) (80,50) (LifeBlock 2)],
  [Block (x,y) (16, 10) (LifeBlock (if odd (floor (x / 40+y / 40)) then 1 else 2))
    | x <- [40, 80 .. windowWidth - 20]
    , y <- [40, 80 .. 100]]
  ]
onPlay :: ApplicationMode -> Bool
onPlay (Stage _) = True
onPlay _         = False
