{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import Actors
import Graphics.Gloss.Interface.IO.Game (playIO)
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
                 tickAvatar
                 0
         . translate 0 350
         . color (makeColor 1 0 0 1)
         $ rectangleSolid 300 700
  where
    tickAvatar _ (useDefaultZOrdering -> me) =
      pure me


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
main = playIO screen
              black
              60
              genesis
              drawGame
              update
              tick


drawGame :: System -> IO Picture
drawGame s = pure
           . scaleToView s
           . uncurry translate (-camera)
           . pictures
           $ roomPic
           : [acts]
  where
    room = s ^. currentRoom
    size = room ^. roomScale
    acts = pictures
         . fmap snd
         . sortBy (comparing fst)
         . fmap (drawActor size)
         $ toListOf (avatar <> currentRoom . actors . traverse) s
    roomPic = uncurry translate
                      (room ^. roomSize  . to (fmap $ (/2) . fromIntegral) . from v2tuple)
            $ room ^. layers
    camera = s ^. cameraPos . _Wrapped' . to (fmap fromIntegral) . from v2tuple


drawActor :: Float -> Actor -> (Int, Picture)
drawActor size sa = (view actorZ sa, )
                  . uncurry translate
                            (worldPos ^. to (fmap fromIntegral) . from v2tuple)
                  . scale size size
                  $ view actorGraphics sa
  where
    worldPos = sa ^. actorPos
