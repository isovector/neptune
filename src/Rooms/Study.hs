{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rooms.Study
  ( studyRoom
  ) where

import LoadImage
import Navigation
import System.FilePath.Posix
import Types


base :: FilePath
base = "assets" </> "Study"

background :: Picture
background = unsafeLoadJpg $ base </> "Background"

regions :: Image PixelRGBA8
regions = unsafeLoadDataPng $ base </> "Regions"


studyRoom :: Room
studyRoom =
  Room background
       (imageSize regions)
       (buildNavMesh regions)
       1
     $ const Nothing

