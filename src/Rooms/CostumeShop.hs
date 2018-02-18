{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rooms.CostumeShop
  ( costumeRoom
  ) where

import Hotspots
import LoadImage
import Navigation
import System.FilePath.Posix
import Types


base :: FilePath
base = "assets" </> "costume"

background :: Form
background = unsafeLoadPng $ base </> "background"
{-# NOINLINE background #-}

-- foreground :: Picture
-- foreground = unsafeLoadPng $ base </> "foreground"

regions :: Image PixelRGBA8
regions = unsafeLoadDataPng $ base </> "regions"
{-# NOINLINE regions #-}

costumeRoom :: Room
costumeRoom =
  Room background
       (imageSize regions)
       (buildNavMesh regions)
       1
     $ mkHotspot regions (== 0x30)
         (Hotspot 0x30 "Door" Touch)
  <||> mkHotspot regions (== 0x66)
         (Hotspot 0x66 "Dress" Examine)

