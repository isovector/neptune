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
    => (forall s. Actor s -> m (Actor s))
    -> SomeActor
    -> m SomeActor
liftActor f (SomeActor actor) = fmap SomeActor $ f actor


------------------------------------------------------------------------------
-- | Run the existentialized 'Actor' tick.
tickActor :: Time -> SomeRoom -> SomeActor -> Game SomeActor
tickActor dt room = liftActor $ flip _onActorTick dt >> pumpMotion dt room

useDefaultZOrdering :: Actor s -> Actor s
useDefaultZOrdering a = a & actorZIndex .~ (10000 - a ^. actorPos'._y)
