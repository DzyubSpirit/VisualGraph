module PhysicalObjects where

import qualified Graphics.Rendering.OpenGL as GL

import Data.Function(on)
import Data.Array

antiGravityConst = 100.0

type Abscissa = Double
type Ordinate = Double
data Vector = Vector Abscissa Ordinate

instance Monoid Vector where
    mempty = Vector 0 0
    mappend (Vector x1 y1) (Vector x2 y2) = Vector (x1+x2) (y1+y2)

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

type Length       = Double
type Elasticity   = Double
type LeftBorder   = Double
type RightBorder  = Double
type TopBorder    = Double
type BottomBorder = Double
data PlayGround = PlayGround {
    resources  :: [Ball],
    processors :: [Ball],
    links      :: Array (Int, Int) (Maybe (Length, Elasticity)),
    borders    :: (LeftBorder, TopBorder, RightBorder, BottomBorder)
}

class Coordinable a where
    getCoords :: a -> Vector
    distanceBetween :: a -> a -> Double
    difference :: a -> a -> Vector
    normDifference :: a -> a -> Vector

    distanceBetween a b = 
        let Vector x1 y1 = getCoords a
            Vector x2 y2 = getCoords b
            dx = x2-x1
            dy = y2-y1
        in sqrt (dx*dx+dy*dy)

    difference = minusVectors `on` getCoords
    normDifference x y = normalizeVector (difference x y)

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
    next dt playGround = putInBorders (borders playGround) $ playGround {
        resources  = resourceBalls',
        processors = processorBalls'
    } where resourceBalls = resources playGround
            processorBalls = processors playGround
            balls = resourceBalls ++ processorBalls
            ballLinks = links playGround 

            resourceBalls'  = processBalls linkResourceForces resourceBalls
            processorBalls' = processBalls linkProcessorForces processorBalls
            
            processBalls x y = zipWith applyForce generalForces y
                where generalForces = zipWith addVectors x $ antigravityForces y

            antigravityForces balls = map antigravityForceForOne balls
            antigravityForceForOne ball = 
              let forces = map (antiGravityForce ball) balls
              in foldr1 addVectors $ forces

            linkResourceForces = map (linkForOne processorBalls True) $ zip [0..] resourceBalls
            linkProcessorForces = map (linkForOne resourceBalls False) $ zip [0..] processorBalls
            
            linkForOne sources isRes (i, target) = 
                let force (j, source) = maybe mempty (linkForce target source) $
                                        ballLinks ! (if isRes then (i,j) else (j,i))
                in foldr1 addVectors . map force . zip [0..] $ sources


            applyForce force ball = next dt $ ball {
              acc = scaleVector (1/(mass ball)) force
            }

addVectors = mappend
minusVectors (Vector x1 y1) (Vector x2 y2) = Vector (x1-x2) (y1-y2)
scaleVector k (Vector x y) = Vector (k*x) (k*y)
normalizeVector v@(Vector 0 0) = v
normalizeVector v@(Vector x y) = scaleVector (1/r) v
    where r = sqrt (x*x+y*y)

antiGravityForce :: Ball -> Ball -> Vector
antiGravityForce ball1 ball2 = (forceFormula `on` mass) ball1 ball2
    where forceFormula m1 m2 = scaleVector f $ normDifference ball1 ball2
            where r = distanceBetween ball1 ball2
                  f = if r /= 0 
                      then antiGravityConst*m1*m2/r/r
                      else 0

linkForce :: Ball -> Ball -> (Length, Elasticity) -> Vector
linkForce ball1 ball2 (len, el) = scaleVector f $ normDifference ball1 ball2
    where r = distanceBetween ball1 ball2
          f = (len-r)*el 

putInBorders (left, top, right, bottom) = 
     modify (\(Vector x y) -> 
                Vector 
                    (max left (min right x))
                    (max top (min bottom y)))
