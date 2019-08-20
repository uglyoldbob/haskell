{-# LANGUAGE FlexibleContexts #-}

module ImageProcess where

import Graphics.UI.Gtk
import Data.Ix (range)
import Data.Word

import Data.Array.MArray (writeArray)
import qualified Data.Array.Repa     as R

import qualified Graphics.Rendering.Cairo as C
import Control.Monad.Trans 

import qualified Codec.Picture as JuicyPixels

type RGB8 = (JuicyPixels.Pixel8, JuicyPixels.Pixel8, JuicyPixels.Pixel8)
type RGB16 = (JuicyPixels.Pixel16, JuicyPixels.Pixel16, JuicyPixels.Pixel16)

fromJuicyPixels8 :: JuicyPixels.Image JuicyPixels.PixelRGB8 -> R.Array R.D R.DIM2 RGB8
fromJuicyPixels8 img = 
    R.fromFunction
        (R.Z R.:. (JuicyPixels.imageWidth img) R.:. (JuicyPixels.imageHeight img))
        (\(R.Z R.:. x R.:. y) ->
            let (JuicyPixels.PixelRGB8 r g b) = JuicyPixels.pixelAt img x y
            in (r, g, b))

fromJuicyPixels16 :: JuicyPixels.Image JuicyPixels.PixelRGB16 -> R.Array R.D R.DIM2 RGB16
fromJuicyPixels16 img = 
    R.fromFunction
        (R.Z R.:. (JuicyPixels.imageWidth img) R.:. (JuicyPixels.imageHeight img))
        (\(R.Z R.:. x R.:. y) ->
            let (JuicyPixels.PixelRGB16 r g b) = JuicyPixels.pixelAt img x y
            in (r, g, b))

toJuicyPixels16 :: R.Array R.D R.DIM2 RGB16 -> JuicyPixels.Image JuicyPixels.PixelRGB16
toJuicyPixels16 array = JuicyPixels.generateImage gen width height
  where
    R.Z R.:. width R.:. height = R.extent array
    gen x y =
      let (r,g,b) = array R.! (R.Z R.:. x R.:. y)
      in JuicyPixels.PixelRGB16 r g b
    

dynWidth :: JuicyPixels.DynamicImage -> Int
dynWidth img = JuicyPixels.dynamicMap JuicyPixels.imageWidth img

dynHeight :: JuicyPixels.DynamicImage -> Int
dynHeight img = JuicyPixels.dynamicMap JuicyPixels.imageHeight img

imageDimensions :: JuicyPixels.DynamicImage -> String
imageDimensions i = ((show (dynWidth i)) ++ " " ++ (show (dynHeight i)))

toWordRGB :: JuicyPixels.PixelRGB8 -> Word32
toWordRGB (JuicyPixels.PixelRGB8 r g b) = 0xff000000 + (bw * (2^16)) + (gw * (2^8)) + rw
    where rw = fromIntegral r
          gw = fromIntegral g
          bw = fromIntegral b

toWordBGR :: JuicyPixels.PixelRGB8 -> Word32
toWordBGR (JuicyPixels.PixelRGB8 r g b) = 0xff000000 + (rw * (2^16)) + (gw * (2^8)) + bw
    where rw = fromIntegral r
          gw = fromIntegral g
          bw = fromIntegral b


convertToPixBuf8 :: JuicyPixels.DynamicImage -> IO Graphics.UI.Gtk.Pixbuf
convertToPixBuf8 img = do
    let width = (dynWidth img)
    let height = (dynHeight img)
    buffer <- pixbufNew ColorspaceRgb True 8 width height
    rowStrideBytes <- pixbufGetRowstride buffer
    let rowStride = rowStrideBytes `quot` 4
    bufferPixels <- pixbufGetPixels buffer :: IO (PixbufData Int Word32)
    let conv = JuicyPixels.convertRGB8 img
    let copyPixel (x, y) = do
        Data.Array.MArray.writeArray bufferPixels (y * rowStride + x) (toWordRGB (JuicyPixels.pixelAt conv x y))
    mapM_ copyPixel $ range ((0, 0), (width-1, height-1))
    return buffer

convertToSurface :: JuicyPixels.DynamicImage -> IO C.Surface
convertToSurface img = do
    let width = (dynWidth img)
    let height = (dynHeight img)
    newSurf <- liftIO $ C.createImageSurface C.FormatRGB24 width height
    surfacePixels <- C.imageSurfaceGetPixels newSurf :: IO (C.SurfaceData Int Word32)
    rowStrideBytes <- C.imageSurfaceGetStride newSurf
    let conv = JuicyPixels.convertRGB8 img
    let rowStride = rowStrideBytes `quot` 4
    let copyPixel (x, y) = do
        Data.Array.MArray.writeArray surfacePixels (y * rowStride + x) (toWordBGR (JuicyPixels.pixelAt conv x y))
    mapM_ copyPixel $ range ((0,0), (width-1, height-1))
    return newSurf

convertToSurfaceDecimate :: JuicyPixels.DynamicImage -> Int -> IO C.Surface
convertToSurfaceDecimate img decimate = do
    let width = (dynWidth img) `quot` decimate
    let height = (dynHeight img) `quot` decimate
    newSurf <- liftIO $ C.createImageSurface C.FormatRGB24 width height
    surfacePixels <- C.imageSurfaceGetPixels newSurf :: IO (C.SurfaceData Int Word32)
    rowStrideBytes <- C.imageSurfaceGetStride newSurf
    let conv = JuicyPixels.convertRGB8 img
    let rowStride = rowStrideBytes `quot` 4
    let copyPixel (x, y) = do
        Data.Array.MArray.writeArray surfacePixels (y * rowStride + x) (toWordBGR (JuicyPixels.pixelAt conv (x*decimate) (y*decimate)))
    mapM_ copyPixel $ range ((0,0), (width-1, height-1))
    return newSurf