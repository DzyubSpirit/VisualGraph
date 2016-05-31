module Graphic.Object where

import qualified Graphics.Rendering.OpenGL as GL
import Data.Function(on)

import Graphic.Drawable

data Object = Circle GL.Position GL.Radius
            | Line GL.Position GL.Position


instance Drawable Object where
    draw (Line p1 p2) = GL.renderPrimitive GL.Lines $
        mapM_ positionToVertex [p1, p2]
    draw (Circle p r) = GL.renderPrimitive GL.LineStrip $
        mapM_ positionToVertex points
            where angles = map ((*(pi/180)) . fromIntegral) [0,3..360]
                  xes = map cos angles
                  yes = map sin angles
                  points = map (add p) $ zipWith (GL.Position `on` (round . (*r))) xes yes

positionToVertex (GL.Position x y) = GL.vertex $ GL.Vertex2 x y
add (GL.Position x1 y1) (GL.Position x2 y2) = GL.Position (x1+x2) (y1+y2)

mult :: Integral a => a -> GL.Position -> GL.Position
mult k (GL.Position x1 y1) = let k' = fromIntegral k in GL.Position (k'*x1) (k'*y1)
