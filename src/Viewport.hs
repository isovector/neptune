{-# LANGUAGE NoImplicitPrelude #-}

module Viewport
  ( virtualView
  , focusCamera
  , viewportScalingFactor
  , getViewport
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


getRoomPicture :: Room -> Picture
getRoomPicture room =
  let size      = view roomSize room
      centerOff = fmap ((/2) . fromIntegral) size
   in uncurry translate ((centerOff & _y *~ -1) ^. from v2tuple)
      $ view layers room

screenToWorld :: Pos -> Game Pos
screenToWorld v2 = do
    vp <- getViewport
    pure $ _y *~ -1
         $ invertViewPort vp (v2 ^. from v2tuple) ^. v2tuple

getViewport :: Game ViewPort
getViewport = do
  room <- getGlobals $ view currentRoom
  vp   <- getGlobals _viewport

  focus <- fmap (fromMaybe zero . listToMaybe) . efor . const $ do
    with hasFocus
    get pos

  let size = room ^. roomSize
      camera = focusCamera size virtualView focus

  pure $ vp
       { viewPortTranslate =
           negate $ (camera & _y *~ -1) ^. from v2tuple
       }

toDrawCoord :: Pos -> Pos
toDrawCoord = _y *~ -1

