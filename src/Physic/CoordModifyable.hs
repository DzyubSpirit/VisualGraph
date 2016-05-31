module Physic.CoordModifyable where

import Physic.Vector

class CoordModifyable a where
    modify :: (Vector -> Vector) -> a -> a

instance CoordModifyable Vector where
    modify f = f
