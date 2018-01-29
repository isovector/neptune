{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}

module System
  ( update
  ) where

import           Graphics.Gloss.Game        (KeyState)
import qualified Graphics.Gloss.Game        as G
import           Types
import           Viewport


data SystemEvent
  = Resize      !(V2 Int)
  | MouseMove   !Pos
  | MouseButton !KeyState
  | SetVerb     !Verb
  | Exit

data SystemAction
  = Click Pos


------------------------------------------------------------------------------
-- | Turn an underlying Gloss event into a system event.
asSystem :: G.Event -> Maybe SystemEvent
asSystem = \case
    G.EventResize v -> Just . Resize    $ v ^. v2tuple
    G.EventMotion v -> Just . MouseMove $ v ^. v2tuple
    G.EventKey (G.MouseButton G.LeftButton) x _ _ -> Just $ MouseButton x
    G.EventKey (G.Char 'e') _ _ _ -> Just $ SetVerb Examine
    G.EventKey (G.Char 't') _ _ _ -> Just $ SetVerb TalkTo
    G.EventKey (G.Char 'u') _ _ _ -> Just $ SetVerb Touch
    G.EventKey (G.Char 'q') _ _ _ -> Just Exit
    _ -> Nothing


--------------------------------------------------------------------------------
---- | Update the system based on underlying Gloss events.
--systemUpdate :: SystemEvent -> System -> (Maybe SystemAction, System)
--systemUpdate = \case
--    Resize s      -> (Nothing, ) . (viewportSize .~ s)
--    MouseMove mm  -> \s -> (Nothing, )
--                         . (mousePos .~ mouseToView s mm)
--                         $ s
--    MouseButton x -> \s -> (s & mouseState .~ x)
--                         & case (s ^. mouseState, x) of
--                             (Up, Down) -> (Just . Click . viewToWorld s $ s ^. mousePos,)
--                             (_, _)     -> (Nothing,)
--    SetVerb verb  -> (Nothing, ) . (nextVerb ?~ verb)
--    Exit          -> error "bye felicia"


------------------------------------------------------------------------------
-- | Top-level update.
update :: G.Event -> GameState -> IO GameState
update ge =
  flip execGame . for_ (asSystem ge) $ \case
    MouseMove v2 -> do
      wp <- screenToWorld v2
      setGlobals $ mousePos .~ wp

    Resize v2 -> do
      setGlobals $ \gs ->
        gs & viewport %~ \vp ->
          vp { viewPortScale = viewportScalingFactor v2
             }

    -- default state
    MouseButton Down -> do
      mouse <- getGlobals $ view mousePos
      getInteractionTarget mouse >>= \case
        Just (InteractionHotspot hs) ->
          -- set a timer
          -- move to a new state
          --   in that state if you see a mouseUp do nothing
          --   if the timer finishes, show the coin in a new state
          --     when you do a mouseUp, check the coordinates on the coin
          --     and issue that verb
          liftIO $ print $ _hsDefaultVerb hs

        Just (InteractionActor e) ->
          liftIO $ print e

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


    Exit -> error "bye felicia"

    _ -> pure ()


getInteractionTarget :: Pos -> Game (Maybe InteractionTarget)
getInteractionTarget p = do
  room <- getGlobals $ view currentRoom
  pure $ InteractionHotspot <$> _hotspots room p

--------------------------------------------------------------------------------
---- | Handler to manage clicks.
--doClick :: Pos -> Game ()
--doClick pos = do
--  s <- ask
--  let next = view nextVerb s
--  fromMaybe (pure ()) . getFirst
--                      $ foldMap First
--    [ case (&& isNothing next) . flip isWalkable pos
--                               $ s ^. currentRoom . navmesh of
--        True  -> Just . modifySystem $ avatar . actorMotion ?~ motion (navigateTo 500 pos)
--        False -> Nothing

--    , case view (currentRoom . hotspots) s pos
--           >>= sequence . ap (,) _onHotspotInteract of
--        Just (hs, f) -> Just . f
--                             $ fromMaybe (_hotspotDefaultVerb hs) next
--        Nothing      -> Nothing
--    ]
--  modifySystem $ nextVerb .~ Nothing

