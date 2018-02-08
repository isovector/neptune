{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LoadImage where

import Types
import Codec.Picture
import System.FilePath.Posix
import System.IO.Unsafe (unsafePerformIO)


unsafeLoadPng :: FilePath -> Form
unsafeLoadPng = sprite . (-<.> "png")

loadPng :: FilePath -> Form
loadPng = sprite . (-<.> "png")


{-# NOINLINE unsafeLoadDataPng #-}
unsafeLoadDataPng :: FilePath -> Image PixelRGBA8
unsafeLoadDataPng fp =
  let Right (ImageRGBA8 img) = unsafePerformIO
                             . readPng
                             $ fp -<.> "png"
   in img

