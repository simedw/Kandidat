module Main where

import Stg.Interpreter
import Stg.Input
import Stg.AST
import Stg.Types
import Stg.Rules
import Stg.Substitution
import Parser.SugarParser
import Util


import Data.Word
import Data.Array.IArray

import Control.Applicative
import Control.Monad

import Graphics.UI.SDL
import Graphics.UI.SDL.Image

import Data.Maybe

import System.Environment

list = [['x','x','o','o']
       ,['x','x','o','o']
       ,['x','x','o','o']
       ,['o','o','o','o']
       ,['o','o','o','o']
       ,['o','o','o','o']
       ]

convert :: SValue String -> [String]
convert (SCon "Cons" [x, ys]) = convert' x : convert ys
convert _                    = []

convert' :: SValue String -> String
convert' (SCon "Cons" [SCon _ [SAtom (AChr x)], vals]) = x : convert' vals
convert'  (SCon "Nil" _)  = []

{-
data SValue t 
  = SAtom (Atom t) -- invariant, this is not an variable
  | SCon t [SValue t]
  | SFun
-} 


advance (Rect x y w h) x' y' = Rect (x+x') (y+y') w h

build lst scr indata w h s = 
    build' lst scr indata (Rect 0 0  (w `div` length indata)
                                     (h `div` length (head indata))) s
  where
    build' lst scr da p s = 
        mapM (\(x,ylst) -> 
            mapM (\(y,p) -> blitSurface (fromJust $ lookup p lst) 
                               Nothing scr 
                               (Just $ Rect (x * w `div` s ) (y * h `div` s) w h)) ylst) $ zip [0..] (map (zip [0..]) indata) 
    {-
    foldM(\ypos ylist -> do
    foldM (\a b -> do
       blitSurface (fromJust $ lookup b lst) Nothing scr (Just a) 
       return (advance a 0 (rectH p))
       ) ypos ylist
    return (advance ypos (rectW p) 0)) p da
    -}

main = withInit [InitEverything] $ do -- withInit calls quit for us.    
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    setCaption "Optimise" []
    
    bgColor  <- (mapRGB . surfaceGetPixelFormat) screen 0xff 0xff 0xff
    clipRect <- Just <$> (getClipRect screen)
    fillRect screen clipRect bgColor
    
    let pfmt = surfaceGetPixelFormat screen
    pcol <- pixelFormatGetColorKey pfmt 
    bpp  <- pixelFormatGetBitsPerPixel pfmt
    alp  <- pixelFormatGetAlpha pfmt

    t1 <- createRGBSurface [SWSurface] 8 8 32 0xFF 0xFF00 0xFF0000 0x00000000
    t2 <- createRGBSurface [SWSurface] 8 8 32 0xFF 0xFF00 0xFF0000 0x00000000
    t3 <- createRGBSurface [SWSurface] 8 8 32 0xFF 0xFF00 0xFF0000 0x00000000
    red   <- mapRGB pfmt 255 0 0 
    green <- mapRGB pfmt 0 255 0 
    blue <- mapRGB pfmt 0 0 255
    black <- mapRGB pfmt 0 0 0
    yellow <- mapRGB pfmt 255 0 255
    
    fillRect t1 Nothing yellow -- red
    fillRect t2 Nothing green
    fillRect t3 Nothing black
    
    let mp = [('X',t1),(' ',t2),('O',t3)]

    optimise : _ <- getArgs
    
--    build [('x',t1),('o',t2)] screen list screenWidth screenHeight
    
    Graphics.UI.SDL.flip screen		
    
    loop (read optimise) screen mp 1
    
 where
    screenWidth  = 500 -- 640
    screenHeight = 500 -- 480
    screenBpp    = 32

    loop opt screen mp s = do -- or whileEvents >>= (Prelude.flip unless) loop 
        setCaption ("Optimise : " ++ show opt ++ "; " ++ show s  ++ "x" ++ show s ) []
        f <- loadFile (LSettings "Prelude.hls" (defaultInput
                        { inputDouble  = Just s
                        }) (not opt)) "Shapes.hls"
        case forceInterpreter f of
            Left err  -> error err
            Right res -> build mp screen (convert res) screenWidth screenHeight (round s)
   
        Graphics.UI.SDL.flip screen		
        quit <- whileEvents
        unless quit (loop opt screen mp (s*2))

    whileEvents = do
        event <- pollEvent
        case event of
            Quit    -> return True
            NoEvent -> return False
            _       -> return False

