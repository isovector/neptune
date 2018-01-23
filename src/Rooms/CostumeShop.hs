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
import Motion
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
costumeRoom = Room []
                background
                (imageSize regions)
                (buildNavMesh regions)
                ( mkHotspot regions (== 48) door
                )
                0.75

door :: Hotspot
door = def
     & onHotspotInteract  ?~ useDoor
     & hotspotId          .~ 96
     & hotspotDefaultVerb .~ Touch
  where
    useDoor Touch   = avatarMotion $ do
      navigateTo 500 . V2 1080 $ 2048 - 1692
      emit . ChangeRoom City $ V2 2424 $ 2048 - 1048
    useDoor Examine = trace "it's a door"
    useDoor TalkTo  = trace "i can't talk to a door"
