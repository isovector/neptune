{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}

module Tick
  ( tick
  ) where

import Data.Ecstasy hiding (runSystem)
-- import Control.Monad.Trans.State (gets)
import Types
import Linear.Metric


------------------------------------------------------------------------------
-- | Update the 'System' with some delta 'Time'.
tick :: Float -> GameState -> IO GameState
tick dt ms@(s, _) = execGame ms $ do
  let room = view currentRoom s
  emap $ do
    src       <- get pos
    NavTo dst <- get pathing

    pure $ defEntity'
      { pathing =
          case navigate (view navmesh room) src dst of
            Just path -> Set $ Following path
            Nothing   -> Unset
      }

  emap $ do
    p                  <- get pos
    Following (g : gs) <- get pathing
    sp                 <- get speed

    let diff = g - p
        dir = normalize diff
        mag = sp * dt
        needed = norm diff
        dp = dir ^* min needed mag
        pos' = p + dp
        epsilon = 2
        shouldChange = needed < epsilon


    pure $ defEntity'
      { pos     = Set $ bool pos' g shouldChange
      , pathing =
        case (shouldChange, gs) of
          (False, _) -> Keep
          (True, []) -> Unset
          (True, _)  -> Set $ Following gs
      }

