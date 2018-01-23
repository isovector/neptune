{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}

module Tick
  ( tick
  ) where

import Data.Ecstasy hiding (runSystem)
import Control.Monad.IO.Class (liftIO)
import Actors
import Control.Monad.Trans.State (gets)
import Hotspots
import System
import Types
import Viewport
import Linear.Metric


------------------------------------------------------------------------------
-- | Update the 'System' with some delta 'Time'.
tick :: Float -> MyState -> IO MyState
tick dt ms@(s, _) = do
  ((_, w'), s') <- runGame' ms $ do
    emap $ do
      p <- get pos
      Following (g : _) <- get pathing
      sp <- get speed
      pure $ defEntity'
        { pos = Set $ p + fmap round (normalize (fmap fromIntegral $ g - p) ^* (sp * dt))
        }

    liftIO . runSystem s $ do
      room <- gets $ view currentRoom
      clock += dt
      avatar %~~ tickActor dt room
      av      <- gets $ view avatar
      cameraPos .= focusCamera (room ^. roomSize) virtualView (av ^. actorPos)


      currentRoom . actors . traverse %~~ tickActor dt room
      lift $ tickHotspots avatar
  -- TODO(sandy): you can get rid of this hack when ecstasy manages everything
  pure (s', w')

