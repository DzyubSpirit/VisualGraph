module Physic.Ball where

import Physic.Vector
import Physic.Coordinable
import Physic.CoordModifyable
import Physic.Timeable

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

instance Coordinable Ball where
    getCoords = coord

instance CoordModifyable Ball where
    modify f ball = ball {
        coord = f (coord ball)
    } 

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
