{-# LANGUAGE NoImplicitPrelude #-}

module Viewport
  ( virtualView
  , viewportScalingFactor
  , viewportScalingFactorInverse
  , scaleToView
  , viewToWorld
  , focusCamera
  ) where

import Types


------------------------------------------------------------------------------
-- | The size of the virtual screen.
virtualView :: V2 Int
virtualView = V2 4096 2048


scaleWithin :: V2 Int -> V2 Int -> Float
scaleWithin x x0 = (\(V2 a b) -> min a b) $ liftA2 (on (/) fromIntegral) x0 x

viewportScalingFactor :: Viewport -> Float
viewportScalingFactor = scaleWithin virtualView . unViewport

scaleToView :: System -> Picture -> Picture
scaleToView s = scale scalingFactor scalingFactor
  where
    scalingFactor = viewportScalingFactor $ s ^. viewportSize

viewportScalingFactorInverse :: Viewport -> Float
viewportScalingFactorInverse = (1 /) . viewportScalingFactor

viewToWorld :: System -> ViewPos -> Pos
viewToWorld s vp = unViewPos $ vp + view cameraPos s


focusCamera :: V2 Int  -- ^ Room size.
            -> V2 Int  -- ^ View size.
            -> Pos     -- ^ Focal point.
            -> ViewPos
focusCamera (fmap fromIntegral -> V2 rx ry)
            (fmap fromIntegral -> V2 sx sy)
            (fmap fromIntegral -> V2 x y) = ViewPos $ fmap (round @Double) result
  where
    w = sx / 2
    h = sy / 2
    result = V2 (clamp' w (rx - w) x) (clamp' h (ry - h) y)

