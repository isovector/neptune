{-# LANGUAGE NoImplicitPrelude #-}

module Tick
  ( tick
  ) where

import Actors
import Control.Monad.Trans.State (gets)
import Hotspots
import System
import Types
import Viewport


------------------------------------------------------------------------------
-- | Update the 'System' with some delta 'Time'.
tick :: Float -> System -> IO System
tick dt = flip runSystem $ do
  room <- gets $ view currentRoom
  clock += dt
  avatar %~~ tickActor dt room
  av      <- gets $ view avatar
  cameraPos .= focusCamera (room ^. roomSize) virtualView (av ^. actorPos)


  currentRoom . actors . traverse %~~ tickActor dt room
  lift $ tickHotspots avatar

