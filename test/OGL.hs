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

--defaultView = View (0) (-0.5) 10 10

prog fun step var opt left right trans = do
    let l = [left,left+step..right]
    res <- G.getList opt (map (+trans) l) fun
    let result = zipWith (\x y -> (realToFrac x, realToFrac y)) l res
    writeIORef var result
    postRedisplay Nothing
    prog fun step var opt left right (trans + step)
{-
prog f step var = do
    let res = map (\x -> (x, f x)) [0,step..5]
    writeIORef var res
    threadDelay 1000000
    postRedisplay Nothing
    prog f (step / 1.2) var
    -}

main = do
    (_, fkn:x:y:w:h:_) <- getArgsAndInitialize
    let view = View (read x) (read y) (read w) (read h * 2)
    let step = read w / 200
    createWindow "OMG Optimise"
    points1 <- newIORef []
    points2 <- newIORef []
    displayCallback $= display fkn view points1 points2
    let right = read x + read w + 1
        left  = read x - 1
    forkOS $ prog fkn step points1 True  left right 0
    forkOS $ prog fkn step points2 False left right 0

    mainLoop


display fkn v points1 points2 = do
    loadIdentity
    clearColor $= Color4 1.0 1.0 1.0 0.0
    clear [ColorBuffer]

    color black
    renderText fkn (-0.9) 0

    cross

    render points1 blue
    renderText "Icke-optimerad" 0 (-0.1)

    translate $ Vector3 (0 :: GLfloat) 1 0

    cross

    render points2 red
    renderText "Optimerad" 0 (-0.1)

    swapBuffers
  where
    render var c = do
        lineWidth $= 3
        color c
        ps <- readIORef var
        renderPrimitive LineStrip $ forM ps (uncurry vert)
    vert x y = vertex $ Vertex3 ((x - viewStartX v) / viewWidth  v * 2 - 1)
                                ((y - viewStartY v) / viewHeight v * 2 - 1)
                                (0.0 :: GLfloat)
    cross = do
      lineWidth $= 1
      color black
      renderPrimitive Lines $ do
        vert 0 (-1000); vert 0 1000
        vert (-1000) 0; vert 1000 0
    renderText text x y = do
      currentRasterPosition $= Vertex4 x y 0 1
      renderString TimesRoman24 text

white, black, red, green, blue :: Color3 GLfloat
white = Color3 1.0 1.0 1.0
black = Color3 0.0 0.0 0.0
red   = Color3 1.0 0.0 0.0
green = Color3 0.0 1.0 0.0
blue  = Color3 0.0 0.0 1.0
