module Main where

import Control.Monad.Trans 
import qualified Graphics.UI.Gtk as G

import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Matrix as M

main = do
        G.initGUI

        -- Create the builder, and load the UI file
        builder <- G.builderNew
        G.builderAddFromFile builder "demo.glade"

        -- Retrieve some objects from the UI
        window <- G.builderGetObject builder G.castToWindow "window1"
        button <- G.builderGetObject builder G.castToButton "button1"
        canvas <- G.builderGetObject builder G.castToDrawingArea "drawingarea1"
        
        G.set window [G.windowTitle G.:= "Example program"]

        -- Basic user interation
        G.on button G.buttonPressEvent $ liftIO G.mainQuit >> return False
        G.on window G.deleteEvent $ liftIO G.mainQuit >> return False
        
        G.on canvas G.draw $ render canvas

        -- Display the window
        G.widgetShowAll window
        G.mainGUI

foreach :: (Monad m) => [a] -> (a -> m b) -> m [b]
foreach = flip mapM
        
render :: G.DrawingArea -> C.Render()
render canvas = do
    C.setSourceRGB 1 0 0
    C.paint
    C.setSourceRGB 1 1 0
    G.Rectangle _x _y w h <- liftIO (G.widgetGetClip canvas)
    grid 50 100 0 $ fromIntegral h

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