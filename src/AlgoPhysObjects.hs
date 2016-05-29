module AlgoPhysObject where

import Graphics.Rendering.OpenGL as GL
import Data.Array
import Data.List((!!))

import PhysicalObjects as PObj
import AlgorithmObjects as AObj
import qualified GraphicalObjects as GObj

data Object = Object {
    graph :: Graph,
    balls :: PObj.PlayGround
} 

linearObjectComposition :: Position -> Position -> Int -> Position
linearObjectComposition x0 dx i = GObj.add x0 $ GObj.mult i dx

defaultResourcePosition :: Int -> Position
defaultResourcePosition = linearObjectComposition (Position 50 50) 
                                                  (Position 0 40)

defaultProcessorPosition :: Int -> Position
defaultProcessorPosition = linearObjectComposition (Position 300 50) 
                                                   (Position 0 40)

makeGraphics :: Object -> GObj.PlayGround
makeGraphics obj = GObj.PlayGround objects
    where (Graph resources processors linkes) = graph obj
          (PObj.PlayGround ballReses ballProcs) = balls obj

          objects = resObjs ++ procObjs ++ linkObjs
          resObjs  = map (makeCircle (Color3 0 0 1)) resPos
          procObjs = map (makeCircle (Color3 0 1 0)) procPos
          linkObjs = [makeLine (Color3 1 1 1) p1 p2 | p1 <- resPos, p2 <- procPos]
          
          positions posFunc array = map (\(i, val) -> posFunc i) $ assocs array
          makeCircle color position = GObj.ColorObject color $
                                      GObj.Circle position 10
          makeLine color p1 p2 = GObj.ColorObject color $
                                 GObj.Line p1 p2

          resPos  = map (toPosition.coord) ballReses
          procPos = map (toPosition.coord) ballProcs