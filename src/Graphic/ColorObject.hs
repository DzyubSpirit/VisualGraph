module Graphic.ColorObject where

import qualified Graphics.Rendering.OpenGL as GL
import Graphic.Drawable
import Graphic.Object

data ColorObject = ColorObject (GL.Color3 GL.GLfloat) Object

instance Drawable ColorObject where
    draw (ColorObject color object) = do
        GL.color color
        draw object
