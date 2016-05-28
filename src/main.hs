import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL(($=))
import Data.IORef
import Data.Array
import Control.Monad
import System.Environment (getArgs, getProgName)

import GraphicalObjects as GObj
import AlgorithmObjects

data Action = Action (IO Action)

main = do
    args <- getArgs
    prog <- getProgName
    main' active

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

    lines <- newIORef []
    run lines

    GLFW.closeWindow
    GLFW.terminate    

active lines = loop waitForPress
    where
        loop action = do
            render lines
            GLFW.swapBuffers
            p <- GLFW.getKey GLFW.ESC
            unless (p == GLFW.Press) $
                do
                    Action action' <- action
                    GLFW.sleep 0.001

                    windowOpen <- getParam Opened
                    unless (not windowOpen) $
                        loop action'

        waitForPress = do
            b <- GLFW.getMouseButton GLFW.ButtonLeft
            case b of
                GLFW.Release -> return (Action waitForPress)
                GLFW.Press   -> do
                    (GL.Position x y) <- GL.get GLFW.mousePos
                    modifyIORef lines (((x,y):) . ((x,y):))
                    return (Action waitForRelease)

        waitForRelease = do
            (GL.Position x y) <- GL.get GLFW.mousePos
            modifyIORef lines (((x,y):).tail)
            b <- GLFW.getMouseButton GLFW.ButtonLeft
            case b of
                GLFW.Release -> return (Action waitForPress)
                GLFW.Press   -> return (Action waitForRelease)

passive lines = do
    GLFW.disableSpecial GLFW.AutoPollEvent

    quit <- newIORef False

    dirty <- newIORef True

    GLFW.windowRefreshCallback $= writeIORef dirty True

    GLFW.keyCallback $= \k s ->
        when (fromEnum k == fromEnum GLFW.ESC && s == GLFW.Press) $
            writeIORef quit True

    GLFW.windowCloseCallback $= (writeIORef quit True >> return True)

    waitForPress dirty
    loop dirty quit
    where
        loop dirty quit = do
            GLFW.waitEvents

            d <- readIORef dirty
            when d $
                render lines >> GLFW.swapBuffers
            writeIORef dirty False
            q <- readIORef quit
            unless q $
                loop dirty quit

        waitForPress dirty =
            do
                GLFW.mousePosCallback $= \_ -> return ()
                GLFW.mouseButtonCallback $= \b s ->
                    when (b == GLFW.ButtonLeft && s == GLFW.Press) $
                        do
                            (GL.Position x y) <- GL.get GLFW.mousePos
                            modifyIORef lines (((x,y):) . ((x,y):))
                            waitForRelease dirty

        waitForRelease dirty =
            do
                GLFW.mousePosCallback $= \(Position x y) ->
                    do
                        modifyIORef lines (((x,y):) . tail)
                        writeIORef dirty True
                GLFW.mouseButtonCallback $= \b s ->
                    when (b == GLFW.ButtonLeft && s == GLFW.Release) $
                        waitForPress dirty

render lines = do
    l <- readIORef lines
    GL.clear [GL.ColorBuffer]
    GL.color $ color3 1 0 0
    GL.renderPrimitive GL.Lines $ mapM_
        (\ (x, y) -> GL.vertex (vertex3 (fromIntegral x) (fromIntegral y) 0)) l
    draw . makeGraphics $ Graph {
        resources  = listArray (0, 2) $ map Resource [1..],
        processors = listArray (0, 1) $ map Processor [1..],
        links      = listArray ((0,0),(2,1)) $ map Link [1..]
    }


vertex3 :: GLfloat -> GLfloat -> GLfloat -> GL.Vertex3 GLfloat
vertex3 = GL.Vertex3

color3 :: GLfloat -> GLfloat -> GLfloat -> GL.Color3 GLfloat
color3 = GL.Color3

