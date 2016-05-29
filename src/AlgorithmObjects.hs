module AlgorithmObjects where

import Data.Array

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