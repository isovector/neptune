{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}

module System
  ( update
  ) where

import           Timers
import           Types
import           Viewport


data SystemEvent
  = Resize      !(V2 Int)
  | MouseMove   !Pos
  | MouseButton !Bool
  | Exit

update :: Monad m => a -> m a
update = pure

--------------------------------------------------------------------------------
---- | Turn an underlying Gloss event into a system event.
--asSystem :: G.Event -> Maybe SystemEvent
--asSystem = \case
--    G.EventResize v -> Just . Resize    $ v ^. v2tuple
--    G.EventMotion v -> Just . MouseMove $ v ^. v2tuple
--    G.EventKey (G.MouseButton G.LeftButton) x _ _ -> Just $ MouseButton x
--    G.EventKey (G.Char 'q') _ _ _ -> Just Exit
--    _ -> Nothing


--------------------------------------------------------------------------------
---- | Top-level update.
--update :: G.Event -> GameState -> IO GameState
--update ge = flip execGame $ do
--  is <- getGlobals $ view gInputDFA

--  for_ ((is,) <$> asSystem ge) $ \case
--    (_, MouseMove v2) -> do
--      wp <- screenToWorld v2
--      setGlobals $ mousePos .~ wp

--    (_, Resize v2) -> do
--      setGlobals $ \gs ->
--        gs & viewport %~ \vp ->
--          vp { viewPortScale = viewportScalingFactor v2
--             }

--    (IStart, MouseButton Down) -> do
--      mouse <- getGlobals $ view mousePos
--      getInteractionTarget mouse >>= \case
--        Just it -> do
--          startTimer TimerCoin 0.5 $ do
--            setGlobals $ gInputDFA .~ ICoinOpen mouse it
--          setGlobals $ gInputDFA .~ IBeforeCoin

--        Nothing -> do
--          room <- getGlobals $ view currentRoom
--          let nav = _navmesh room
--          case isWalkable nav mouse of
--            True ->
--              emap $ do
--                with isAvatar
--                pure defEntity'
--                  { pathing = Set $ NavTo mouse
--                  }
--            False -> pure ()

--    (IBeforeCoin, MouseButton Up) -> do
--      cancelTimer TimerCoin
--      setGlobals $ gInputDFA .~ IStart

--    (ICoinOpen p it, MouseButton Up) -> do
--      mouse <- getGlobals $ view mousePos
--      let verb = getBBSurface (coinSurface p) mouse
--      for_ verb $ doInteraction it
--      setGlobals $ gInputDFA .~ IStart


--    (_, Exit) -> error "bye felicia"

--    _ -> pure ()


--doInteraction :: InteractionTarget -> Verb -> Game ()
--doInteraction _ = liftIO . print


--coinSurface :: Pos -> BBSurface Verb
--coinSurface p = BBSurface
--  [ ( moveBB (V2 (-width) 0 + p) rect
--    , TalkTo
--    )
--  , ( moveBB p rect
--    , Examine
--    )
--  , ( moveBB (V2 width 0 + p) rect
--    , Touch
--    )
--  ]
--  where
--    width = 48
--    rect = rectBB width width


--getInteractionTarget :: Pos -> Game (Maybe InteractionTarget)
--getInteractionTarget p = do
--  room <- getGlobals $ view currentRoom
--  pure $ InteractionHotspot <$> _hotspots room p

