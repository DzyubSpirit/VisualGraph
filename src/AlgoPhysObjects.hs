module AlgoPhysObject where

import Graphics.Rendering.OpenGL as GL
import Data.Array
import Data.List((!!))
import Data.Maybe(maybe, isJust)

import PhysicalObjects as PObj
import AlgorithmObjects as AObj
import qualified GraphicalObjects as GObj

data Object = Object {
    graph :: Graph,
    balls :: PObj.PlayGround
} 

instance CoordModifyable Object where
    modify f obj = obj {
        balls = modify f (balls obj) 
    }


instance Timeable Object where
    next dt obj = obj {
        balls = next dt (balls obj)
    }

makeGraphics :: Object -> GObj.PlayGround
makeGraphics obj = GObj.PlayGround objects
    where (Graph resources processors links) = graph obj
          playGround = balls obj
          ballReses  = PObj.resources playGround
          ballProcs  = PObj.processors playGround

          objects = resObjs ++ procObjs ++ linkObjs
          resObjs  = map (makeCircle (Color3 0 0 1)) resPos
          procObjs = map (makeCircle (Color3 0 1 0)) procPos
          linkObjs = [makeLine (Color3 1 1 1) p1 p2 | (i, p1) <- zip [0..] resPos, 
                                                      (j, p2) <- zip [0..] procPos,
                                                      isJust (links ! (i, j))]
          
          positions posFunc array = map (\(i, val) -> posFunc i) $ assocs array
          makeCircle color position = GObj.ColorObject color $
                                      GObj.Circle position 10
          makeLine color p1 p2 = GObj.ColorObject color $
                                 GObj.Line p1 p2

          resPos  = map (toPosition.coord) ballReses
          procPos = map (toPosition.coord) ballProcs