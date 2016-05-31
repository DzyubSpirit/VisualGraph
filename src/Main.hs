module Main where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL(($=))
import Data.IORef
import Data.Array
import Control.Monad
import System.Environment (getArgs, getProgName)

import Physic.PlayGround as P
import Physic.Vector
import Physic.Ball
import Physic.Timeable
import Graphic.PlayGround as G
import Graphic.Drawable
import Algorithm.Graph as A
import GraphBalls as GB

data Action = Action (IO Action)

main = do
    args <- getArgs
    prog <- getProgName
    main' active

windowWidth = 640
windowHeight = 480

main' run = do
    GLFW.initialize
    GLFW.openWindow (GL.Size 640 480) [GLFW.DisplayAlphaBits 8] GLFW.Window
    GLFW.windowTitle $= "GLFW Demo"
    GL.shadeModel $= GL.Smooth

    GL.lineSmooth $= GL.Enabled
    GL.blend      $= GL.Enabled
    GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.lineWidth  $= 1.5

    GL.clearColor $= Color4 0 0 0 0

    GLFW.windowSizeCallback $= \ size@(GL.Size w h) ->
        do
            GL.viewport $= (GL.Position 0 0, size)
            GL.matrixMode $= GL.Projection
            GL.loadIdentity
            GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

    let makeStandartBalls = map (\(x,y) -> Ball {
            coord = Vector x y,
            speed = Vector 0 0,
            acc   = Vector 0 0,
            radius = 10,
            mass  = 1
        })
    playGround <- newIORef $ GraphBalls (Graph {
        A.resources  = listArray (0, 2) $ map Resource [1..],
        A.processors = listArray (0, 1) $ map Processor [1..],
        A.links      = listArray ((0,0),(2,1)) $ map (\x -> if x == 0 then Nothing else Just $ Link x) 
                                                    [1, 5, 0, 7, 0, 2]
    }) (P.PlayGround {
        P.resources  = makeStandartBalls [(50, 50), (50, 100),(50,150)], 
        P.processors = makeStandartBalls [(150, 30), (150, 60)],
        P.links      = listArray ((0,0),(2,1)) $ map (\x -> if x == 0 then Nothing else Just (50, 0.001)) 
                                                    [1, 5, 0, 7, 0, 2],
        P.borders    = (0, 0, windowWidth, windowHeight)
    })

    run playGround

    GLFW.closeWindow
    GLFW.terminate    


active playGround = loop waitForPress
    where
        loop action = do
            render playGround
            GLFW.swapBuffers
            p <- GLFW.getKey GLFW.ESC
            unless (p == GLFW.Press) $
                do
                    Action action' <- action
                    GLFW.sleep 0.001

                    windowOpen <- getParam Opened
                    unless (not windowOpen) $ do
                        modifyIORef playGround $ next 1
                        loop action'

        waitForPress = do
            b <- GLFW.getMouseButton GLFW.ButtonLeft
            case b of
                GLFW.Release -> return (Action waitForPress)
                GLFW.Press   -> do
                    (GL.Position x y) <- GL.get GLFW.mousePos
                    return (Action waitForRelease)

        waitForRelease = do
            (GL.Position x y) <- GL.get GLFW.mousePos
            b <- GLFW.getMouseButton GLFW.ButtonLeft
            case b of
                GLFW.Release -> return (Action waitForPress)
                GLFW.Press   -> return (Action waitForRelease)

render playGround = do
    pg <- readIORef playGround
    GL.clear [GL.ColorBuffer]
    draw . makeGraphics $ pg

vertex3 :: GLfloat -> GLfloat -> GLfloat -> GL.Vertex3 GLfloat
vertex3 = GL.Vertex3

color3 :: GLfloat -> GLfloat -> GLfloat -> GL.Color3 GLfloat
color3 = GL.Color3

