{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rooms.Study
  ( studyRoom
  ) where

import Actors
import Hotspots
import LoadImage
import Motion
import Navigation
import System.FilePath.Posix
import Types


base :: FilePath
base = "assets" </> "Study"

background :: Picture
background = unsafeLoadJpg $ base </> "Background"

regions :: Image PixelRGBA8
regions = unsafeLoadDataPng $ base </> "Regions"


instance HasRoom 'Study where
  type RoomModel 'Study = ()

studyRoom :: Room 'Study
studyRoom = Room [SomeActor charles]
                 ()
                 background
                 (imageSize regions)
                 (buildNavMesh regions)
                 ( mkHotspot regions (== 48) door
              <||> mkHotspot regions (== 96) mirror
              <||> mkHotspot regions (== 144) carpet
                 )
                 1

door :: Hotspot
door = def
     & onHotspotInteract  ?~ useDoor
     & hotspotId          .~ 48
     & hotspotDefaultVerb .~ Touch
  where
    useDoor :: Verb -> Game ()
    useDoor Touch   = avatarMotion $ do
      navigateTo 500 $ V2 1200 200
      emit . ChangeRoom City $ V2 3500 200
    useDoor Examine = trace "it's a door"
    useDoor TalkTo  = trace "i can't talk to a door"

mirror :: Hotspot
mirror = def
       & onHotspotInteract ?~ (\v -> trace ("mirror " <> show v))
       & hotspotId         .~ 96

carpet :: Hotspot
carpet = def
       & onHotspotEnter .~ trace "nice rug"
       & onHotspotLeave .~ trace "it was a nice rug"
       & hotspotId      .~ 144


charles :: Actor ()
charles = Actor (V2 2900 250)
                Nothing
                Nothing
                (const $ pure . useDefaultZOrdering)
                ()
                0
        . translate 0 150
        . color (makeColor 0 0 1 1)
        $ circleSolid 150
