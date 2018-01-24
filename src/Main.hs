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
mainChar = Actor (V2 120 20)
                 Nothing
                 Nothing
                 (const pure)
                 0
         . translate 0 35
         . color (makeColor 1 0 0 1)
         $ rectangleSolid 30 70


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
            { pos = Just $ V2 120 20
            , gfx = Just
                  . translate 0 15
                  . color (makeColor 0 0 1 1)
                  $ circleSolid 15
            , speed = Just 50
            , pathing = Just $ NavTo $ V2 400 20
            , hasFocus = Just ()
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

  focus <- fmap (fromMaybe zero . listToMaybe) . efor . const $ do
    with hasFocus
    get pos

  pure . scaleToView s
       . uncurry translate (negate $ camera focus)
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
    camera focus = focusCamera (room ^. roomSize) virtualView focus
                ^. _Wrapped'
                 . to (fmap fromIntegral)
                 . from v2tuple


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
