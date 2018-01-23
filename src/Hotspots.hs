{-# LANGUAGE NoImplicitPrelude #-}

module Hotspots
  ( mkHotspot
  , tickHotspots
  , (<||>)
  ) where

import Codec.Picture.Types (pixelAt, PixelRGBA8(..))
import Types


------------------------------------------------------------------------------
-- | Get the byte of a pixel corresponding to hotspot data.
getHotspotByte :: PixelRGBA8 -> Word8
getHotspotByte (PixelRGBA8 _ g _ _) = g


------------------------------------------------------------------------------
-- | Construct a hotspot lookup function by way of a pixel filter over a region
-- image.
mkHotspot
    :: Image PixelRGBA8
    -> (Word8 -> Bool)
    -> Hotspot
    -> Pos
    -> Maybe Hotspot
mkHotspot img f h = bool Nothing (Just h)
                  . f
                  . getHotspotByte
                  . uncurry (pixelAt img)
                  . view (from v2tuple)
                  . clampToWorld
                  . (_y %~ (z -))
  where
    imgSize@(V2 _ z) = imageSize img - 1
    clampToWorld = clamp (V2 0 0) imgSize


------------------------------------------------------------------------------
-- | Update the active hotspots.
tickHotspots :: Lens' System SomeActor -> Game ()
tickHotspots l = do
  s <- ask
  case view l s of
    SomeActor actor ->
      case ( view activeHotspot s
          , view (currentRoom . hotspots) s $ _actorPos' actor
          ) of
        (Just hs, Just hs') | hs /= hs'
                           -> _onHotspotLeave hs
                           >> _onHotspotEnter hs'
                           >> modifySystem (activeHotspot ?~ hs')

        (Just hs, Nothing) -> _onHotspotLeave hs
                           >> modifySystem (activeHotspot .~ Nothing)

        (Nothing, Just hs) -> _onHotspotEnter hs
                           >> modifySystem (activeHotspot ?~ hs)

        _                  -> pure ()


------------------------------------------------------------------------------
-- | Lifted 'Alternative' operator '(<|>)'.
(<||>) :: Alternative f => (a -> f b) -> (a -> f b) -> a -> f b
(<||>) = liftA2 (<|>)

infixl 3 <||>

