{-# LANGUAGE NoImplicitPrelude #-}

module Viewport
  ( virtualView
  , focusCamera
  , viewportScalingFactor
  , getRoomPicture
  , screenToWorld
  , toDrawCoord
  ) where

import Types


------------------------------------------------------------------------------
-- | The size of the virtual screen.
virtualView :: V2 Int
virtualView = V2 613 205

viewportScalingFactor
    :: V2 Int  -- ^ Viewport actual size.
    -> Float
viewportScalingFactor = scaleWithin virtualView
  where
    scaleWithin x x0 = (\(V2 a b) -> min a b)
                     $ liftA2 (on (/) fromIntegral)
                              x0
                              x

focusCamera :: V2 Int  -- ^ Room size.
            -> V2 Int  -- ^ View size.
            -> Pos     -- ^ Focal point.
            -> Pos
focusCamera (fmap fromIntegral -> V2 rx ry)
            (fmap fromIntegral -> V2 sx sy)
            (V2 x y) = result
  where
    w = sx / 2
    h = sy / 2
    result = V2 (clamp' w (rx - w) x) (clamp' h (ry - h) y)


getRoomPicture :: Room -> Form
getRoomPicture room =
  let size      = view roomSize room
      centerOff = fmap ((/2) . fromIntegral) size
   in move (centerOff & _y *~ -1)
      $ view layers room

screenToWorld :: Pos -> Game Pos
screenToWorld = pure

toDrawCoord :: Pos -> Pos
toDrawCoord = _y *~ -1

