{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}

module System
  ( update
  ) where

import Controller
import Timers
import Types
import Viewport


data SystemEvent
  = Resize    !(V2 Int)
  | MouseEdge !Bool
  | Exit
  deriving (Show)


getSystemEvent :: Game (Maybe SystemEvent)
getSystemEvent = do
  ctrl  <- getGlobals $ view gController
  ctrl' <- getController
  setGlobals $ gController .~ ctrl'

  pure $
    case (risingEdge ctrl ctrl', fallingEdge ctrl ctrl') of
      (True, False) -> Just $ MouseEdge True
      (False, True) -> Just $ MouseEdge False
      _ -> Nothing


------------------------------------------------------------------------------
-- | Top-level update.
update :: Game ()
update = do
  se <- getSystemEvent
  is <- getGlobals $ view gInputDFA

  for_ (fmap (is,) se) $ \case
    (_, Resize v2) -> do
      pure ()
      -- setGlobals $ \gs ->
      --   gs & viewport %~ \vp ->
      --     vp { viewPortScale = viewportScalingFactor v2
      --        }

    (IStart, MouseEdge True) -> do
      mouse <- getMousePos
      getInteractionTarget mouse >>= \case
        Just it -> do
          startTimer TimerCoin 0.5 $ do
            setGlobals $ gInputDFA .~ ICoinOpen mouse it
          setGlobals $ gInputDFA .~ IBeforeCoin

        Nothing -> do
          room <- getGlobals $ view currentRoom
          let nav = _navmesh room
          case isWalkable nav mouse of
            True ->
              emap $ do
                with isAvatar
                pure defEntity'
                  { pathing = Set $ NavTo mouse
                  }
            False -> pure ()

    (IBeforeCoin, MouseEdge False) -> do
      cancelTimer TimerCoin
      setGlobals $ gInputDFA .~ IStart

    (ICoinOpen p it, MouseEdge False) -> do
      mouse <- getMousePos
      let verb = getBBSurface (coinSurface p) mouse
      for_ verb $ doInteraction it
      setGlobals $ gInputDFA .~ IStart


    (_, Exit) -> error "bye felicia"

    _ -> pure ()


doInteraction :: InteractionTarget -> Verb -> Game ()
doInteraction _ = liftIO . print


coinSurface :: Pos -> BBSurface Verb
coinSurface p = BBSurface
  [ ( moveBB (V2 (-width) 0 + p) rect
    , TalkTo
    )
  , ( moveBB p rect
    , Examine
    )
  , ( moveBB (V2 width 0 + p) rect
    , Touch
    )
  ]
  where
    width = 48
    rect = rectBB width width


getInteractionTarget :: Pos -> Game (Maybe InteractionTarget)
getInteractionTarget p = do
  room <- getGlobals $ view currentRoom
  pure $ InteractionHotspot <$> _hotspots room p

