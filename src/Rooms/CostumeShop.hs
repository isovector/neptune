{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rooms.CostumeShop
  ( costumeRoom
  ) where

import LoadImage
import Navigation
import System.FilePath.Posix
import Types


base :: FilePath
base = "assets" </> "costume"

background :: Picture
background = unsafeLoadPng $ base </> "background"

-- foreground :: Picture
-- foreground = unsafeLoadPng $ base </> "foreground"

regions :: Image PixelRGBA8
regions = unsafeLoadDataPng $ base </> "regions"

costumeRoom :: Room
costumeRoom = Room
                background
                (imageSize regions)
                (buildNavMesh regions)
                0.75

