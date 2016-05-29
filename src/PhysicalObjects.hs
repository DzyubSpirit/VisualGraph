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
type Mass = Double
data Ball = Ball {
    coord :: Coord,
    speed :: Speed,
    mass  :: Mass
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

processTime :: Double -> [Ball] -> [Ball]
processTime dt balls = zipWith applyForce forces balls
    where forceForOne ball = 
            let forces = map (forceBetween ball)
            in foldr1 addVectors $ forces balls
          forces = map forceForOne balls
          applyForce force ball =
            let addedSpeed = scaleVector (dt/(mass ball)) force
                newSpeed = addVectors (speed ball) addedSpeed
                addedCoord = scaleVector dt newSpeed
                newCoord = addVectors (coord ball) addedCoord
            in ball {
                coord = newCoord,
                speed = newSpeed
            }

