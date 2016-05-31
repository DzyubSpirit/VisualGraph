module Physic.Vector where

type Abscissa = Double
type Ordinate = Double
data Vector = Vector Abscissa Ordinate

instance Monoid Vector where
    mempty = Vector 0 0
    mappend (Vector x1 y1) (Vector x2 y2) = Vector (x1+x2) (y1+y2)

addVectors :: Vector -> Vector -> Vector
addVectors x y = mappend x y
minusVectors (Vector x1 y1) (Vector x2 y2) = Vector (x1-x2) (y1-y2)
scaleVector k (Vector x y) = Vector (k*x) (k*y)
normalizeVector v@(Vector 0 0) = v
normalizeVector v@(Vector x y) = scaleVector (1/r) v
    where r = sqrt (x*x+y*y)
