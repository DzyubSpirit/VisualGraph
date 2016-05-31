module GraphBalls where

import Graphics.Rendering.OpenGL as GL
import Data.Array
import Data.List((!!))
import Data.Maybe(maybe, isJust)
import Data.Monoid(Sum)

import Physic.PlayGround as P
import Physic.CoordModifyable
import Physic.Timeable
import Physic.Vector as P
import Physic.Ball
import Algorithm.Graph as A
import Graphic.PlayGround as G
import Graphic.ColorObject
import Graphic.Object as G

toPosition (P.Vector x y) = GL.Position (round x) (round y)

data GraphBalls = GraphBalls {
    graph :: Graph,
    balls :: P.PlayGround
} 

instance CoordModifyable GraphBalls where
    modify f obj = obj {
        balls = modify f (balls obj) 
    }


instance Timeable GraphBalls where
    next dt obj = obj {
        balls = next dt (balls obj)
    }

readGraphBalls borders str = 
    let firstLine:linksStr =  lines str
        n:_ = map readInt.words $ firstLine
        links = map (map readDouble.words) linksStr
        
        readDouble = read :: String -> Double
        readInt = read :: String -> Int 

        clinks = concat links

        count   = fromIntegral . length . 
                  filter (/= 0) $ clinks
        meanLink = (sum clinks) / count
        lengthD = fromIntegral . length
        resArray = listArray (0, n-1) resList 
        
        resList = map Resource resDList  
        resDList = 
            map (func2.func1) links
                where func1 = filter (/= 0)
                      func2 res' = ((+5).(*10)) $ sum res'/ meanLink / (lengthD res')
        procArray = listArray (0, n-1) procList
        procList = map Processor procDList
        procDList = 
            map (func1.func2) [0..n-1]
                where func1 res' = ((+5).(*10)) $ sum res'/ meanLink / (lengthD res') 
                      func2 i = filter (/= 0) $ map (!!i) links
        linkValues = listArray ((0,0),(n-1,n-1)) clinks
        links' = fmap mapFunc linkValues
            where mapFunc x = if x/=0 then Just (Link x False) else Nothing
    
        makeStandartBalls = map (\(x,y,r) -> Ball {
            coord = Vector x y,
            speed = mempty,
            acc   = mempty,
            radius =  r,
            mass  = r
        })

    in GraphBalls {
        graph = calculateSolution $ Graph {
            A.resources = resArray,
            A.processors = procArray,
            A.links = links'
        },
        balls = P.PlayGround {
            P.resources = makeStandartBalls $ zip3 (repeat 50) 
                                                 ([50,100..])
                                                 resDList,
            P.processors = makeStandartBalls $ zip3 (repeat 150) 
                                                  ([50,100..])
                                                  procDList,
            P.links = fmap (\x -> if x/=0 then Just (50, 0.001) else Nothing) linkValues,
            P.borders = borders

        }
    }

changeBorders borders graphBalls = putInBorders borders $ 
    graphBalls {
        balls = curBalls {
                borders = borders
            } 
    } where
        curBalls = balls graphBalls


makeGraphics :: GraphBalls -> G.PlayGround
makeGraphics obj = G.PlayGround objects
    where (Graph resources processors links) = graph obj
          playGround = balls obj
          ballReses  = P.resources playGround
          ballProcs  = P.processors playGround

          objects = linkObjs ++ resObjs ++ procObjs
          resObjs  = map (makeCircle (Color3 0 0 1)) $ zip resPos resRadius
          procObjs = map (makeCircle (Color3 0 1 0)) $ zip procPos procRadius
          linkObjs = [makeLine (links!(i,j)) p1 p2 | (i, p1) <- zip [0..] resPos,
                                                      (j, p2) <- zip [0..] procPos,
                                                      isJust (links ! (i, j))]
          
          positions posFunc array = map (\(i, val) -> posFunc i) $ assocs array
          makeCircle color (position, radius) = ColorObject color $
                                      Circle position radius
          makeLine (Just (Link _ s)) p1 p2 = ColorObject 
                                            (if s 
                                             then  Color3 1 1 1
                                             else Color3 0.75 0.25 0.25) $
                                             G.Line p1 p2

          resPos  = map getPos ballReses
          procPos = map getPos ballProcs
          getPos = toPosition . coord 
          resRadius = map radius ballReses
          procRadius = map radius ballProcs