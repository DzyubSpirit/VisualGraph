module PhysicalObjects where

import qualified Graphics.Rendering.OpenGL as GL

import Data.Function(on)
import Data.Array

antiGravityConst = 1.0

type Abscissa = Double
type Ordinate = Double
data Vector = Vector Abscissa Ordinate

toPosition (Vector x y) = GL.Position (round x) (round y)

type Coord = Vector
type Speed = Vector
type Acceleration = Vector
type Mass = Double
type Radius = Double
data Ball = Ball {
    coord  :: Coord,
    speed  :: Speed,
    acc    :: Acceleration,
    radius :: Radius,
    mass   :: Mass
}

data PlayGround = PlayGround {
    resources  :: [Ball],
    processors :: [Ball]
}

class Coordinable a where
    getCoords :: a -> Vector
    distanceBetween :: a -> a -> Double
    difference :: a -> a -> Vector

    distanceBetween a b = 
        let Vector x1 y1 = getCoords a
            Vector x2 y2 = getCoords b
            dx = x2-x1
            dy = y2-y1
        in sqrt (dx*dx+dy*dy)

    difference = minusVectors `on` getCoords

instance Coordinable Vector where
    getCoords = id

instance Coordinable Ball where
    getCoords = coord

class CoordModifyable a where
    modify :: (Vector -> Vector) -> a -> a

instance CoordModifyable Vector where
    modify f = f

instance CoordModifyable Ball where
    modify f ball = ball {
        coord = f (coord ball)
    } 

instance CoordModifyable PlayGround where
    modify f playGround = playGround {
        resources  = map (modify f) (resources playGround),
        processors = map (modify f) (processors playGround)
    }

type Time = Double
class Timeable a where
    next :: Time -> a -> a

instance Timeable Ball where
    next dt ball@(Ball coord speed acc _ _) = 
        let addedSpeed = scaleVector dt acc
            newSpeed = addVectors speed addedSpeed
            addedCoord = scaleVector dt newSpeed
            newCoord = addVectors coord addedCoord
        in ball {
            coord = newCoord,
            speed = newSpeed
        }

instance Timeable PlayGround where
    next dt playGround = playGround {
        resources  = applyForces (resources playGround),
        processors = applyForces (processors playGround)
    } where balls = (resources playGround) ++ (processors playGround)
            forceForOne ball = 
              let forces = map (forceBetween ball)
              in foldr1 addVectors $ forces balls
            forces balls = map forceForOne balls
            applyForce force ball = next dt $ ball {
              acc = scaleVector (1/(mass ball)) force
            }
            applyForces affectedBalls = zipWith applyForce 
                (forces affectedBalls) affectedBalls

addVectors (Vector x1 y1) (Vector x2 y2) = Vector (x1+x2) (y1+y2)
minusVectors (Vector x1 y1) (Vector x2 y2) = Vector (x1-x2) (y1-y2)
scaleVector k (Vector x y) = Vector (k*x) (k*y)
normalizeVector v@(Vector 0 0) = v
normalizeVector v@(Vector x y) = scaleVector (1/r) v
    where r = sqrt (x*x+y*y)

forceBetween :: Ball -> Ball -> Vector
forceBetween ball1 ball2 = (forceFormula `on` mass) ball1 ball2
    where r = distanceBetween ball1 ball2
          forceFormula m1 m2 = scaleVector f . normalizeVector $
                               difference ball1 ball2
            where f = if r /= 0 
                      then antiGravityConst*m1*m2/r/r
                      else 0

putInBorders (left, right) (top, bottom) = 
     modify (\(Vector x y) -> 
                Vector 
                    (max left (min right x))
                    (max top (min bottom y)))
