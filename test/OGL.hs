module Main where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import qualified Graph as G

data View = View
    { viewStartX :: GLfloat
    , viewStartY :: GLfloat
    , viewWidth  :: GLfloat
    , viewHeight :: GLfloat
    }

defaultView = View (0) (-0.5) 10 10

prog fun step var opt = do
    let l = [0,step..50]
    res <- G.getList opt l fun
    let result = zipWith (\x y -> (realToFrac x, realToFrac y)) l res
    writeIORef var result
    postRedisplay Nothing
    prog fun (step / 1.2) var opt
{-
prog f step var = do
    let res = map (\x -> (x, f x)) [0,step..5]
    writeIORef var res
    threadDelay 1000000
    postRedisplay Nothing
    prog f (step / 1.2) var
    -}

main = do
    (_, fkn:_) <- getArgsAndInitialize
    createWindow "OMG Optimise"
    points1 <- newIORef []
    points2 <- newIORef []
    displayCallback $= display defaultView points1 points2
    forkOS $ prog fkn 1 points1 True
    forkOS $ prog fkn 1 points2 False
    mainLoop


display v points1 points2 = do
    loadIdentity
    clearColor $= Color4 1.0 1.0 1.0 0.0
    clear [ColorBuffer]
    color black
    renderString TimesRoman24 "foobar"
    renderPrimitive Lines $ do
        vert 0 (-1000); vert 0 1000
        vert (-1000) 0; vert 1000 0
    render points1 blue
    translate $ Vector3 (0 :: GLfloat) 0.2 0
    render points2 red
    swapBuffers
  where
    render var c = do
        color c
        ps <- readIORef var
        renderPrimitive LineStrip $ forM ps (uncurry vert)
    vert x y = vertex $ Vertex3 ((x - viewStartX v) / viewWidth  v * 2 - 1)
                                ((y - viewStartY v) / viewHeight v * 2 - 1)
                                (0.0 :: GLfloat)

white, black, red, green, blue :: Color3 GLfloat
white = Color3 1.0 1.0 1.0
black = Color3 0.0 0.0 0.0
red   = Color3 1.0 0.0 0.0
green = Color3 0.0 1.0 0.0
blue  = Color3 0.0 0.0 1.0
