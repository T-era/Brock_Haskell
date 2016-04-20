module BBase(Pos(..), Ball(..), Board(..), Reflector(rect, cols), reflect) where
import Data.IORef

maxSpeed dx dy = speed2 > 5.0
  where
    speed2 = dx * dx + dy * dy

newtype Pos d = Pos (d, d) deriving (Show, Eq, Ord)

data Ball = Ball { pos :: (Double, Double), dir :: (Double, Double) }
data Board = Board { bPos :: Double, width :: Double }

class Reflector a where
  rect :: a -> (Pos Double, (Double, Double))
  cols :: a -> Maybe a

instance Reflector Board where
  rect (Board x width) = (Pos (x, 293), (width, 3))
  cols = return

reflect :: Reflector a => Double -> (a, Ball) -> (Maybe a, Ball)
reflect rndT (reflector, ball@(Ball (x, y) (dx,dy))) = _reflect (rect reflector)
  where
    _reflect (Pos (rx, ry), (width, height))
      | nohit     = (Just reflector, ball)
      | edgeHitLtRb = (cols reflector, ball { dir = diffuse rndT (dy, dx)})  -- 方角入れ替え
      | edgeHitLbRt = (cols reflector, ball { dir = diffuse rndT (-dy, -dx)})  -- 方角入れ替え
      | sideHit   = (cols reflector, ball { dir = (-dx,dy * 0.9) })
      | otherwise = (cols reflector, ball { dir = (dx * 0.9,-dy) })
      where
        nohit = outside y (ry - height) (ry + height)
            || outside x (rx - width) (rx + width)
        sideHit
          | dx == 0  = False
          | dx < 0 = _sideHit (rx + width)
          | dx > 0 = _sideHit (rx - width)
        _sideHit wallX
          | hitY <= ry + height && hitY > ry - height = True
          | otherwise = False
          where
            hitY = (wallX - x) * dy / dx + y
        edgeHitLtRb = edgeHit [(rx - width, ry - height, (\ dx dy -> dx >= 0 && dy >= 0)),
                               (rx + width, ry + height, (\ dx dy -> dx <= 0 && dy <= 0))]
        edgeHitLbRt = edgeHit [(rx - width, ry + height, (\ dx dy -> dx >= 0 && dy <= 0)),
                               (rx + width, ry - height, (\ dx dy -> dx <= 0 && dy >= 0))]
        edgeHit edges = any isOn edges
        isOn (x', y', dJudge) = (((rx - x') * dy / dx) `neq` (dy - y')
            && dJudge dx dy)
neq a b = abs (a - b) < 0.3

diffuse :: Double -> (Double, Double) -> (Double, Double)
diffuse rndT (dx, dy)
  | maxSpeed dx dy = (dx', dy')
  | otherwise      = (_accelerate dx', _accelerate dy')
  where
    (dx', dy') = (dx * cos rndT, sin rndT * dy)
    _accelerate d = d * 1.2

outside a minA maxA = a < minA || a > maxA
