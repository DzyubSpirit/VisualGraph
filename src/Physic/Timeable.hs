module Physic.Timeable where

type Time = Double
class Timeable a where
    next :: Time -> a -> a