{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rooms.City
  ( cityRoom
  ) where

import LoadImage
import Navigation
import System.FilePath.Posix
import Types


base :: FilePath
base = "assets" </> "city"

background :: Form
background = unsafeLoadPng $ base </> "background"

regions :: Image PixelRGBA8
regions = unsafeLoadDataPng $ base </> "regions"


cityRoom :: Room
cityRoom =
  Room background
       (imageSize regions)
       (buildNavMesh regions)
       0.5
     $ const Nothing

