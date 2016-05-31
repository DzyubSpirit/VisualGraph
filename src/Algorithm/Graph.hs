module Algorithm.Graph where

import Data.Array

type HardLevel = Double
type Power = Double
type Selected = Bool
newtype Resource  = Resource  HardLevel
newtype Processor = Processor Power
data Link      = Link Double Selected

data Graph = Graph {
    resources  :: Array Int Resource,
    processors :: Array Int Processor,
    links      :: Array (Int, Int) (Maybe Link)
}

calculateSolution :: Graph -> Graph
calculateSolution graph = graph {
        links = array (bounds mat) linkList'
    } where mat = links graph
            linkList = assocs mat
            linkList' = undefined
            --linkList' = map linkFunc linkList
            --linkFunc ((i,j), link) = 
            --    if i == j
            --    then ((i,j),fmap selectLink link)
            --    else ((i,j), link) 
            --selectLink (Link v _) = Link v True

solution :: Array (Int, Int) (Maybe Link) -> Array (Int, Int) (Maybe Link)
solution arr = undefined
    where arrList = assocs arr
          (n, m) = zipTuples $ bounds arr
          arr' = []

zipTuples (a, b) (c, d) = ((a,c),(b,d))