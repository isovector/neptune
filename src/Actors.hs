{-# LANGUAGE NoImplicitPrelude #-}

module Actors
  ( tickActor
  , useDefaultZOrdering
  ) where

import Motion
import Types

------------------------------------------------------------------------------
-- | Run the existentialized 'Actor' tick.
tickActor :: Time -> Room -> Actor -> Game Actor
tickActor dt room = pumpMotion dt room

useDefaultZOrdering :: Actor -> Actor
useDefaultZOrdering a = a & actorZ .~ (10000 - a ^. actorPos._y)

