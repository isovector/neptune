{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import Graphics.Gloss.Interface.IO.Game (playIO)
import Data.Ecstasy hiding (System)
import Graphics.Gloss
import System
import Types
import Rooms
import Tick
import Viewport


screen :: Display
screen = InWindow "ward" (640, 480) (1000,1000)


mainChar :: Actor
mainChar = Actor (V2 1200 200)
                 Nothing
                 Nothing
                 (const pure)
                 0
         . translate 0 350
         . color (makeColor 1 0 0 1)
         $ rectangleSolid 300 700


genesis :: System
genesis = def
        & avatar        .~ mainChar
        & currentRoomId .~ CostumeShop
        & rooms         .~ [ (Study,       studyRoom)
                           , (City,        cityRoom)
                           , (CostumeShop, costumeRoom)
                           ]
        & cameraPos     .~ ViewPos zero


main :: IO ()
main = do
  s' <- execGame' (genesis, (0, defWorld)) $ do
          void $ newEntity $ defEntity
            { pos = Just $ V2 2424 $ 2048 - 1048
            , gfx = Just
                  . translate 0 150
                  . color (makeColor 0 0 1 1)
                  $ circleSolid 150
            , speed = Just 500
            , pathing = Just $ Following [V2 0 0]
            }

  playIO screen
         black
         60
         s'
         drawGame
         update
         tick


drawGame :: MyState -> IO Picture
drawGame ms@(s, _) = evalGame' ms $ do
  gfxs <- efor . const $
    (,) <$> get pos <*> get gfx

  pure . scaleToView s
       . uncurry translate (-camera)
       . pictures
       $ roomPic
       : [acts gfxs]
  where
    room = s ^. currentRoom
    size = room ^. roomScale
    acts gfxs = pictures
              . (++ fmap (uncurry $ drawGfx size) gfxs)
              . fmap snd
              . sortBy (comparing fst)
              . fmap (drawActor size)
              $ toListOf (avatar <> currentRoom . actors . traverse) s
    roomPic = uncurry translate
                      (room ^. roomSize  . to (fmap $ (/2) . fromIntegral) . from v2tuple)
            $ room ^. layers
    camera = s ^. cameraPos . _Wrapped' . to (fmap fromIntegral) . from v2tuple


drawGfx :: Float -> Pos -> Picture -> Picture
drawGfx size worldPos pic =
  uncurry translate (worldPos ^. to (fmap fromIntegral) . from v2tuple)
    $ scale size size pic


drawActor :: Float -> Actor -> (Int, Picture)
drawActor size sa = (view actorZ sa, )
                  . uncurry translate
                            (worldPos ^. to (fmap fromIntegral) . from v2tuple)
                  . scale size size
                  $ view actorGraphics sa
  where
    worldPos = sa ^. actorPos
