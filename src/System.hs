{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}

module System
  ( update
  ) where

import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Reader (runReaderT)
import           Control.Monad.Trans.State  (StateT (..), execStateT, get,
                                             modify, put)
import           Control.Monad.Writer       (runWriter)
import           Graphics.Gloss.Game        (KeyState)
import qualified Graphics.Gloss.Game        as G
import           Types
import           Viewport


data SystemEvent
  = Resize      !Pos
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
    G.EventResize v -> Nothing -- Just . Resize    $ v ^. v2tuple . from _Wrapped'
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
update _ ms  = pure ms
  -- let (action, s') = fromMaybe (Nothing, s)
  --                  . fmap (flip systemUpdate s)
  --                  $ asSystem e
  -- fmap (, w) . runGame s' $
  --   case action of
  --     Just (Click pos) -> doClick pos >> pure s'
  --     _                -> pure s'


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

