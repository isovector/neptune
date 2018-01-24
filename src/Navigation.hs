{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ViewPatterns               #-}

module Navigation
  ( buildNavMesh
  ) where

import           Codec.Picture.Types (Image (..), pixelAt, PixelRGBA8(..))
import           Control.Lens
import           Control.Monad (guard)
import           Data.Bits (testBit)
import           Data.Graph.AStar (aStar)
import qualified Data.HashSet as HS
import           Data.Hashable
import           Data.Monoid (All (..))
import qualified Data.Vector as V
import           Linear.Metric (distance, normalize)
import           Linear.V2 (V2 (..), _x, _y)
import           Linear.Vector
import           Types hiding (trace)


newtype Node = Node { unNode :: Int } deriving (Num, Integral, Enum, Real, Eq, Ord, Hashable, Show)
newtype Img  = Img  { unImg  :: Int } deriving (Num, Integral, Enum, Real, Eq, Ord, Hashable, Show)



------------------------------------------------------------------------------
-- | Construct a 'NavMesh' from an 'Image Pixel8'.
buildNavMesh :: Image PixelRGBA8 -> NavMesh
buildNavMesh img = NavMesh {..}
  where
    navigate (clampToWorld . fmap fromIntegral -> a)
             (clampToWorld . fmap fromIntegral -> b) = smoothPath img a b
                                                     . fmap (worldSpace h) <$>
      let nava = navSpace h a
          navb = navSpace h b
      in if nava == navb && not (canWalkOn img nava)
            then Nothing
            else aStar neighbors
                        dist
                        (flip dist navb)
                        (== navb)
                        nava

    clampToWorld = clamp (V2 0 0) (fmap fromIntegral $ imageSize img - 1)
    isWalkable = canWalkOn img . navSpace h . clampToWorld . fmap fromIntegral

    -- The (exclusive) max for width and height of a stage.
    w, h :: Node
    V2 w h = navBounds img

    dist :: V2 Node -> V2 Node -> Float
    dist a b = distance (fmap fromIntegral a) (fmap fromIntegral b)

    -- Get the neighbors of a navigation point.
    neighbors :: V2 Node -> HS.HashSet (V2 Node)
    neighbors v2 = HS.fromList $ do
      V2 x y <- fmap (v2 &)
                [ _x -~ 1
                , _x +~ 1
                , _y -~ 1
                , _y +~ 1
                ]
      guard $ canWalkOn img v2
      guard $ x >= 0
      guard $ x <= w
      guard $ y >= 0
      guard $ y <= h
      guard . canWalkOn img $ V2 x y
      return $ V2 x y

smoothPath :: Image PixelRGBA8 -> V2 Float -> V2 Float -> [V2 Float] -> [Pos]
smoothPath img src dst path =
    let v = V.fromList $ (src : path) ++ [dst]
     in fmap round <$> go 0 (V.length v - 1) v
  where
    go l u v =
      if sweepWalkable img (v V.! l) (v V.! u)
         then [v V.! u]
         else let mid = ((u - l) `div` 2) + l
               in go l mid v ++ go mid u v


navBounds :: Image a -> V2 Node
navBounds = subtract 1
          . fmap (Node . floor)
          . (^/ resolution)
          . fmap fromIntegral
          . imageSize

------------------------------------------------------------------------------
-- | Compute a walkability sweep by quantizing points on the sweep line to the
-- nearest nav nodes. Returns true iff all nodes near the path are walkable.
sweepWalkable :: Image PixelRGBA8 -> V2 Float -> V2 Float -> Bool
sweepWalkable img src dst =
  let dir   = normalize $ dst - src
      distInNodeUnits = round $ distance src dst
      bounds@(V2 _ z) = navBounds img
    in getAll . flip foldMap [0 .. distInNodeUnits] $ \n ->
        let me = src + dir ^* (fromIntegral @Int n)
          in All . canWalkOn img $ clamp (V2 0 0) bounds $ navSpace z me


walkableBit :: Int
walkableBit = 7


getWalkableByte :: PixelRGBA8 -> Word8
getWalkableByte (PixelRGBA8 _ _ b _) = b

------------------------------------------------------------------------------
-- | A bitmap is walkable if it has bit 7 (eg >= 128) set.
canWalkOn :: Image PixelRGBA8 -> V2 Node -> Bool
canWalkOn img (V2 x y) = flip testBit walkableBit
                       . getWalkableByte
                       . pixelAt img ((unImg $ imgSpace x) + floor (resolution / 2))
                       $ (unImg $ imgSpace y) + floor (resolution / 2)


------------------------------------------------------------------------------
-- | Scale a nav point up to world space.
worldSpace :: Node -> V2 Node -> V2 Float
worldSpace y = (^* resolution) . fmap fromIntegral . (_y %~ (y -))


------------------------------------------------------------------------------
-- | Scale a world point down to navigation space.
navSpace :: Node -> V2 Float -> V2 Node
navSpace y = (_y %~ (y -)) . fmap (Node . floor) . (^/ resolution)


------------------------------------------------------------------------------
-- | Scale a nav point up to image space (integral, but full-resolution).
imgSpace :: Node -> Img
imgSpace = round . (* resolution) . fromIntegral . unNode


------------------------------------------------------------------------------
-- | The resolution at which we sample an image for navigation points.
resolution :: Float
resolution = 3.2

