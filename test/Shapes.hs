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

build lst scr indata w h = 
    build' lst scr indata (Rect 0 0  (w `div` length indata)
                                     (h `div` length (head indata)))
  where
    build' lst scr da p = foldM(\ypos ylist -> do
    foldM (\a b -> do
       blitSurface (fromJust $ lookup b lst) Nothing scr (Just a) 
       return (advance a 0 (rectH p))
       ) ypos ylist
    return (advance ypos (rectW p) 0)) p da
    

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

    t1 <- createRGBSurface [SWSurface] 500 500 32 0xFF 0xFF00 0xFF0000 0x00000000
    t2 <- createRGBSurface [SWSurface] 500 500 32 0xFF 0xFF00 0xFF0000 0x00000000
    t3 <- createRGBSurface [SWSurface] 500 500 32 0xFF 0xFF00 0xFF0000 0x00000000
    red   <- mapRGB pfmt 255 0 0 
    green <- mapRGB pfmt 0 255 0 
    blue <- mapRGB pfmt 0 0 255
    
    fillRect t1 Nothing red
    fillRect t2 Nothing green
    fillRect t3 Nothing blue
    
    let mp = [('X',t1),(' ',t2),('O',t3)]


    
--    build [('x',t1),('o',t2)] screen list screenWidth screenHeight
    
    Graphics.UI.SDL.flip screen		
    
    loop screen mp 1
    
 where
    screenWidth  = 640
    screenHeight = 480
    screenBpp    = 32

    loop screen mp s = do -- or whileEvents >>= (Prelude.flip unless) loop 
        f <- loadFile (LSettings "Prelude.hls" (defaultInput
                        { inputDouble  = Just s
                        }) False) "Shapes.hls"
        case forceInterpreter f of
            Left err  -> error err
            Right res -> build mp screen (convert res) screenWidth screenHeight
   
        Graphics.UI.SDL.flip screen		
        quit <- whileEvents
        unless quit (loop screen mp (s*2))

    whileEvents = do
        event <- pollEvent
        case event of
            Quit    -> return True
            NoEvent -> return False
            _       -> return False

