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
import UglyStatistics

main = do
        G.initGUI

        -- Create the builder, and load the UI file
        builder <- G.builderNew
        G.builderAddFromFile builder "reliability.glade"

        -- Retrieve some objects from the UI
        window <- G.builderGetObject builder G.castToWindow "window1"
        button <- G.builderGetObject builder G.castToButton "button1"
        canvas <- G.builderGetObject builder G.castToDrawingArea "drawingarea1"
        text1 <- G.builderGetObject builder G.castToTextView "textview1"
        label <- G.builderGetObject builder G.castToLabel "label1"
        
        G.set window [G.windowTitle G.:= "Example program"]

        -- Basic user interation
        G.on button G.buttonPressEvent $ liftIO $ G.labelSetLabel label thinkObedience >> return False
        G.on window G.deleteEvent $ liftIO G.mainQuit >> return False

        tb <- G.textViewGetBuffer text1
        G.on tb G.bufferChanged $ liftIO $ do
            things <- textViewString text1
            G.labelSetLabel label things
        
        let list = [1,6,34,5,6,3,5,67,5,3]
        G.on canvas G.draw $ render3 canvas list               

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

textViewString :: G.TextViewClass self => self -> IO String
textViewString tv = do
    tb <- G.textViewGetBuffer tv
    startiter <- G.textBufferGetStartIter tb
    enditer <- G.textBufferGetEndIter tb
    contents <- (G.textBufferGetText tb startiter enditer False) :: IO String
    return contents
        
foreach :: (Monad m) => [a] -> (a -> m b) -> m [b]
foreach = flip mapM

render3 :: G.DrawingArea -> [Int] -> C.Render()
render3 canvas list = do
    C.setSourceRGB 1 1 1
    C.paint
    C.setSourceRGB 0 0 0
    G.Rectangle _x _y w h <- liftIO (G.widgetGetClip canvas)
    C.translate 0 $ (fromIntegral (h))
    C.scale 1 (-1)
    grid 0 (fromIntegral w) 0 (fromIntegral h)
    let calcm = (fromIntegral $ maximum list) * 1.1
    plotDots $ prepareDots 0 ((fromIntegral w)/calcm) 0 ((fromIntegral h)*0.9) $ discreteCDF list 5

prepareDot x1 x2 y1 y2 (x, y) = ((x+x1)*x2, (y+y1)*y2)

prepareDots x1 x2 y1 y2 l = map (prepareDot x1 x2 y1 y2) l

plotDot (x, y) = do
    C.arc (x) (y) 1 0 (2*pi)
    C.strokePreserve
    C.fill

plotDots l = sequence_ $ map plotDot l

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