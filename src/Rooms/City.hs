{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rooms.City
  ( cityRoom
  ) where

import Hotspots
import LoadImage
import Navigation
import System.FilePath.Posix
import Motion
import Types


base :: FilePath
base = "assets" </> "city"

background :: Picture
background = unsafeLoadPng $ base </> "background"

regions :: Image PixelRGBA8
regions = unsafeLoadDataPng $ base </> "regions"


cityRoom :: Room
cityRoom = Room []
                background
                (imageSize regions)
                (buildNavMesh regions)
                ( mkHotspot regions (== 48) window
             <||> mkHotspot regions (== 96) door
                )
                0.5

door :: Hotspot
door = def
     & onHotspotInteract  ?~ useDoor
     & hotspotId          .~ 96
     & hotspotDefaultVerb .~ Touch
  where
    useDoor Touch   = avatarMotion $ do
      navigateTo 500 $ V2 3500 200
      emit . ChangeRoom Study $ V2 1200 200
    useDoor Examine = trace "it's a door"
    useDoor TalkTo  = trace "i can't talk to a door"

window :: Hotspot
window = def
       & onHotspotInteract ?~ (const . changeRoom CostumeShop . V2 1080 $ 2048 - 1692)
       & hotspotId         .~ 48
