module AlgorithmObjects where

import Graphics.Rendering.OpenGL as GL
import Data.Array
import Data.List((!!))

import GraphicalObjects as GObj

type HardLevel = Double
type Power = Double
newtype Resource  = Resource  HardLevel
newtype Processor = Processor Power
newtype Link      = Link Double

data Graph = Graph {
    resources  :: Array Int Resource,
    processors :: Array Int Processor,
    links      :: Array (Int, Int) Link
}

linearObjectComposition :: Position -> Position -> Int -> Position
linearObjectComposition x0 dx i = add x0 $ mult i dx

defaultResourcePosition :: Int -> Position
defaultResourcePosition = linearObjectComposition (Position 50 50) 
                                                  (Position 0 40)

defaultProcessorPosition :: Int -> Position
defaultProcessorPosition = linearObjectComposition (Position 300 50) 
                                                   (Position 0 40)


makeGraphics :: Graph -> PlayGround
makeGraphics (Graph resources processors links) = PlayGround objects
    where objects = resObjs ++ procObjs ++ linkObjs
          resObjs  = map (makeCircle (Color3 0 0 1)) resPos
          procObjs = map (makeCircle (Color3 0 1 0)) procPos
          linkObjs = [makeLine (Color3 1 1 1) p1 p2 | p1 <- resPos, p2 <- procPos]
          
          positions posFunc array = map (\(i, val) -> posFunc i) $ assocs array
          makeCircle color position = ColorObject color $
                                      Circle position 10
          makeLine color p1 p2 = ColorObject color $
                                 GObj.Line p1 p2

          resPos  = positions defaultResourcePosition resources
          procPos = positions defaultProcessorPosition processors