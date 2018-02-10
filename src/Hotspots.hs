{-# LANGUAGE NoImplicitPrelude #-}

module Hotspots
  ( mkHotspot
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
                  . fmap round
  where
    clampToWorld = clamp (V2 0 0)
                 . fmap fromIntegral
                 $ imageSize img - 1


------------------------------------------------------------------------------
-- | Lifted 'Alternative' operator '(<|>)'.
(<||>) :: Alternative f => (a -> f b) -> (a -> f b) -> a -> f b
(<||>) = liftA2 (<|>)

infixl 3 <||>

