module Physic.PlayGround where

import Data.Function(on)
import Data.Array

import Physic.Vector
import Physic.CoordModifyable
import Physic.Timeable
import Physic.Ball
import Physic.Coordinable

antiGravityConst = 1.0
wallForceConst   = 50.0

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

instance CoordModifyable PlayGround where
    modify f playGround = playGround {
        resources  = map (modify f) (resources playGround),
        processors = map (modify f) (processors playGround)
    }


instance Timeable PlayGround where
    next dt playGround = putInBorders borders' $ playGround {
        resources  = resourceBalls',
        processors = processorBalls'
    } where borders' = borders playGround

            resourceBalls = resources playGround
            processorBalls = processors playGround
            balls = resourceBalls ++ processorBalls
            ballLinks = links playGround 

            resourceBalls'  = processBalls linkResourceForces resourceBalls
            processorBalls' = processBalls linkProcessorForces processorBalls
            
            processBalls x y = zipWith applyForce generalForces y
                where generalForces = zipWith addVectors 
                                        (zipWith addVectors x $ antigravityForces y)
                                        (wallForces y)
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


            wallForces balls = map (wallForceForOne borders') balls;
            wallForceForOne (left, top, right, bottom) ball = foldr1 addVectors forces
                where (Vector x y) = coord ball
                      formula r = if r == 0
                                  then 0
                                  else wallForceConst/r/r
                      forces = zipWith scaleVector 
                                (map formula [x-left, y-top, x-right, y-bottom])
                                [Vector 1 0, Vector 0 1, Vector (-1) 0, Vector 0 (-1)]

            applyForce force ball = next dt $ ball {
              acc = scaleVector (1/(mass ball)) force
            }


putInBorders (left, top, right, bottom) = 
     modify (\(Vector x y) -> 
                Vector 
                    (max left (min right x))
                    (max top (min bottom y)))


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
