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


mainChar :: Actor Float
mainChar = Actor (V2 1200 200)
                 Nothing
                 Nothing
                 tickAvatar
                 3
                 0
         . translate 0 350
         . color (makeColor 1 0 0 1)
         $ rectangleSolid 300 700
  where
    tickAvatar dt (useDefaultZOrdering -> me) = do
      let state = me ^. actorState
      case state <= 0 of
        True -> do
          trace "three seconds!"
          pure $ me & actorState .~ 3
        False ->
          pure $ me & actorState -~ dt


genesis :: System
genesis = def
        & avatar        .~ SomeActor mainChar
        & currentRoomId .~ CostumeShop
        & rooms         .~ [ (Study,       SomeRoom studyRoom)
                           , (City,        SomeRoom cityRoom)
                           , (CostumeShop, SomeRoom costumeRoom)
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


drawActor :: Float -> SomeActor -> (Int, Picture)
drawActor size sa = (view actorZ sa, )
                  . uncurry translate
                            (worldPos ^. to (fmap fromIntegral) . from v2tuple)
                  . scale size size
                  $ view actorGraphics sa
  where
    worldPos = sa ^. actorPos