module Graphic.Drawable where

class Drawable a where
    draw :: a -> IO ()
