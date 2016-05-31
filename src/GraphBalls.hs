module GraphBalls where

import Graphics.Rendering.OpenGL as GL
import Data.Array
import Data.List((!!))
import Data.Maybe(maybe, isJust)

import Physic.PlayGround as P
import Physic.CoordModifyable
import Physic.Timeable
import Physic.Vector as P
import Physic.Ball
import Algorithm.Graph as A
import Graphic.PlayGround as G
import Graphic.ColorObject
import Graphic.Object as G

toPosition (P.Vector x y) = GL.Position (round x) (round y)

data GraphBalls = GraphBalls {
    graph :: Graph,
    balls :: P.PlayGround
} 

instance CoordModifyable GraphBalls where
    modify f obj = obj {
        balls = modify f (balls obj) 
    }


instance Timeable GraphBalls where
    next dt obj = obj {
        balls = next dt (balls obj)
    }

makeGraphics :: GraphBalls -> G.PlayGround
makeGraphics obj = G.PlayGround objects
    where (Graph resources processors links) = graph obj
          playGround = balls obj
          ballReses  = P.resources playGround
          ballProcs  = P.processors playGround

          objects = resObjs ++ procObjs ++ linkObjs
          resObjs  = map (makeCircle (Color3 0 0 1)) resPos
          procObjs = map (makeCircle (Color3 0 1 0)) procPos
          linkObjs = [makeLine (Color3 1 1 1) p1 p2 | (i, p1) <- zip [0..] resPos, 
                                                      (j, p2) <- zip [0..] procPos,
                                                      isJust (links ! (i, j))]
          
          positions posFunc array = map (\(i, val) -> posFunc i) $ assocs array
          makeCircle color position = ColorObject color $
                                      Circle position 10
          makeLine color p1 p2 = ColorObject color $
                                 G.Line p1 p2

          resPos  = map (toPosition . coord) ballReses
          procPos = map (toPosition . coord) ballProcs