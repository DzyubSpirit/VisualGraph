module PhysicalObjects where

import Data.Function(on)
import Data.Array

antiGravityConst = 1.0

type Abscissa = Double
type Ordinate = Double
data Vector = Vector Abscissa Ordinate

type Coord = Vector
type Speed = Vector
type Mass = Double
data Ball = Ball {
    coord :: Coord,
    speed :: Speed,
    mass  :: Mass
}

class Coordinable a where
    getCoords :: a -> Vector
    distanceBetween :: a -> a -> Double
    
    distanceBetween a b = 
        let Vector x1 y1 = getCoords a
            Vector x2 y2 = getCoords b
            dx = x2-x1
            dy = y2-y1
        in sqrt (dx*dx+dy*dy)

instance Coordinable Vector where
    getCoords = id

instance Coordinable Ball where
    getCoords = coord

forceBetween :: Ball -> Ball -> Double
forceBetween ball1 ball2 = (forceFormula `on` mass) ball1 ball2
    where r = distanceBetween ball1 ball2
          forceFormula m1 m2 = antiGravityConst*m1*m2/r/r

processTime :: Double -> Array Int Ball -> Array Int Ball
processTime dt balls = undefined