module Graphic.PlayGround where

import Graphic.ColorObject
import Graphic.Drawable

newtype PlayGround = PlayGround [ColorObject]

instance Drawable PlayGround where
    draw (PlayGround objs) = mapM_ draw objs