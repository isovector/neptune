{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}

module Tick
  ( tick
  ) where

import System
import Data.Ecstasy hiding (runSystem)
-- import Control.Monad.Trans.State (gets)
import Types
import Linear.Metric
import Timers
import Scripting
import Utils


------------------------------------------------------------------------------
-- | Update the 'System' with some delta 'Time'.
tick :: Time -> Game ()
tick dt = do
  s <- getGlobals id

  mouse <- getMousePos
  int <- getInteractionTarget mouse

  emap $ do
    with isNarration
    pure $ defEntity'
      { gfx =
          case int of
            Just (InteractionHotspot hs) ->
              Set $ renderText (rgb 0 0 0) $ _hsName hs
            _ -> Unset
      }

  updateTimers dt

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

