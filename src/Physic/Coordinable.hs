module Physic.Coordinable where

import Data.Function(on)

import Physic.Vector

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
