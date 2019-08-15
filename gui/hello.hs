--static compile options
-- -optl-mwindows -threaded
module Main where

import Control.Monad.Trans 
import qualified Graphics.UI.Gtk as G

import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Matrix as M
import qualified Data.Array.MArray

import Control.Concurrent
import System.Random
import System.IO

import qualified Codec.Picture as JuicyPixels

import TestModule
import ImageProcess

main = do
        G.initGUI

        -- Create the builder, and load the UI file
        builder <- G.builderNew
        G.builderAddFromFile builder "demo.glade"

        -- Retrieve some objects from the UI
        window <- G.builderGetObject builder G.castToWindow "window1"
        button <- G.builderGetObject builder G.castToButton "button1"
        canvas <- G.builderGetObject builder G.castToDrawingArea "drawingarea1"
        checkbox <- G.builderGetObject builder G.castToCheckButton "checkbutton1"
        label <- G.builderGetObject builder G.castToLabel "label1"
        
        G.set window [G.windowTitle G.:= "Example program"]

        -- Basic user interation
        G.on button G.buttonPressEvent $ liftIO $ G.labelSetLabel label thinkObedience >> return False
        G.on window G.deleteEvent $ liftIO G.mainQuit >> return False
        
        imgz <- JuicyPixels.readImage "DSC_0408-01.jpg"
        
        case imgz of
            Left x -> G.postGUIAsync $ G.labelSetText label (show "Error")
            Right i -> do
                jimbob <- liftIO $ convertToSurface i
                G.on canvas G.draw $ renderSurface canvas jimbob
                
                whatever <- liftIO $ convertToPixBuf8 i
                --G.on canvas G.draw $ render2 canvas whatever
                G.postGUIAsync $ G.labelSetText label $ imageDimensions i

        forkIO $ do
            let
                printTime t = do{
                    threadDelay 5000000;
                    G.postGUIAsync $ G.labelSetText label (show t);
                    printTime (t+1)}
            printTime 0
        

        -- Display the window
        G.widgetShowAll window
        G.mainGUI

foreach :: (Monad m) => [a] -> (a -> m b) -> m [b]
foreach = flip mapM

renderSurface :: G.DrawingArea -> C.Surface -> C.Render()
renderSurface canvas surf = do
    C.setSourceSurface surf 0 0
    C.paint

render2 :: G.DrawingArea -> G.Pixbuf -> C.Render()
render2 canvas pb = do
    G.setSourcePixbuf pb 0 0
    C.paint
    
render :: G.DrawingArea -> Either String JuicyPixels.DynamicImage -> C.Render()
render canvas img = do
    case img of
        Left x -> do
            C.setSourceRGB 1 0 0
            C.paint
            C.setSourceRGB 1 1 0
            G.Rectangle _x _y w h <- liftIO (G.widgetGetClip canvas)
            grid 50 100 0 $ fromIntegral h
        Right i -> do
            test <- liftIO $ C.createImageSurface C.FormatRGB24 50 50
            C.setSourceSurface test 0 0
            C.paint

-- Grid and axes
grid xmin xmax ymin ymax = do
  -- axes
  C.moveTo 0 ymin; C.lineTo 0 ymax; C.stroke
  C.moveTo xmin 0; C.lineTo xmax 0; C.stroke
  -- grid
  C.setDash [0.01, 0.99] 0
  foreach [xmin .. xmax] $ \ x ->
      do C.moveTo x ymin
         C.lineTo x ymax
         C.stroke
  C.setDash [] 0