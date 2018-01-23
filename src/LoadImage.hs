{-# LANGUAGE LambdaCase #-}

module LoadImage where

import Codec.Picture
import Graphics.Gloss
import Graphics.Gloss.Game(jpg, png)
import System.FilePath.Posix
import System.IO.Unsafe (unsafePerformIO)


{-# NOINLINE unsafeLoadJpg #-}
unsafeLoadJpg :: FilePath -> Picture
unsafeLoadJpg = jpg . (-<.> "jpg")


{-# NOINLINE unsafeLoadPng #-}
unsafeLoadPng :: FilePath -> Picture
unsafeLoadPng = png . (-<.> "png")


{-# NOINLINE unsafeLoadDataPng #-}
unsafeLoadDataPng :: FilePath -> Image PixelRGBA8
unsafeLoadDataPng fp =
  let Right (ImageRGBA8 img) = unsafePerformIO
                             . readPng
                             $ fp -<.> "png"
   in img

