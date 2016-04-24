module BBase(Pos, Dir, Vect, Rect, Ball(..), Board(..), Reflector(rect, cols), reflect, windowWidth, windowHeight) where
import Data.IORef

(windowWidth, windowHeight) = (300, 300) :: (Double, Double)

maxSpeed dx dy = speed2 > 5.0
  where
    speed2 = dx * dx + dy * dy

type Pos = (Double, Double)
type Dir = (Double, Double)
type Vect = (Double, Double)
type Rect = (Pos, Dir)

data Ball = Ball { pos :: Pos, dir :: Dir } deriving Show
data Board = Board { bPos :: Double, width :: Double } deriving Show

class Reflector a where
  rect :: a -> Rect
  cols :: a -> Maybe a

instance Reflector Board where
  rect (Board x width) = ((x, 293), (width, 3))
  cols = return

reflect :: Reflector a => Double -> (a, Ball) -> (Maybe a, Ball)
reflect rndT (reflector, ball) = _reflect (rect reflector)
  where
    (Ball (x, y) (dx,dy)) = ball
    _reflect ((rx, ry), (width, height))
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
        isOn (x', y', dJudge) = (((x - x') * dy / dx) `neq` (y - y')
            && dJudge dx dy)
neq a b = abs (a - b) < 10.3

diffuse :: Double -> Vect -> Vect
diffuse rndT (dx, dy)
  | maxSpeed dx dy = (dx', dy')
  | otherwise      = (_accelerate dx', _accelerate dy')
  where
    (dx', dy') = (cosT * dx - sinT * dy, sinT * dx + cosT * dy)
    sinT = sin rndT
    cosT = cos rndT
    _accelerate d = d * 1.2

outside a minA maxA = a < minA || a > maxA
