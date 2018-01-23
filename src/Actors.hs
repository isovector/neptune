{-# LANGUAGE NoImplicitPrelude #-}

module Actors
  ( tickActor
  , liftActor
  , useDefaultZOrdering
  ) where

import Motion
import Types


------------------------------------------------------------------------------
-- | Lift a function over an 'Actor' to 'SomeActor'.
liftActor
    :: Functor m
    => (Actor -> m Actor)
    -> Actor
    -> m Actor
liftActor = id


------------------------------------------------------------------------------
-- | Run the existentialized 'Actor' tick.
tickActor :: Time -> Room -> Actor -> Game Actor
tickActor dt room = liftActor $ flip _onActorTick dt >> pumpMotion dt room

useDefaultZOrdering :: Actor -> Actor
useDefaultZOrdering a = a & actorZ .~ (10000 - a ^. actorPos._y)

