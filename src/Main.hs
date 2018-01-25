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
screen = InWindow "ward" (640, 480) (0, 0)


defGlobals :: Globals
defGlobals = Globals
  { _rooms = [ (Study,       studyRoom)
             , (City,        cityRoom)
             , (CostumeShop, costumeRoom)
             ]
  , _currentRoomId = CostumeShop
  , _mousePos      = zero
  , _mouseState    = Up
  , _viewport      = viewPortInit
  , _nextVerb      = Nothing
  }


main :: IO ()
main = do
  s' <- execGame (defGlobals, (0, defWorld)) $ do
          void $ newEntity $ defEntity
            { pos = Just $ V2 0 0
            , gfx = Just
                  . translate 0 15
                  . color (makeColor 0 0 1 1)
                  $ circleSolid 15
            , speed = Just 50
            -- , pathing = Just $ NavTo $ V2 400 20
            , hasFocus = Just ()
            }

  playIO screen
         black
         60
         s'
         drawGame
         update
         tick


drawGame :: GameState -> IO Picture
drawGame ms = evalGame ms $ do
  room <- getGlobals $ view currentRoom
  vp   <- getViewport
  gfxs <- efor . const $
    (,) <$> fmap (_y *~ -1) (get pos)
        <*> get gfx
  let pic = getRoomPicture room

  let size = room ^. roomScale
  pure . applyViewPortToPicture vp
       . pictures
       $ pic
       : fmap (uncurry $ drawGfx size) gfxs


drawGfx :: Float -> Pos -> Picture -> Picture
drawGfx size worldPos pic =
  uncurry translate (worldPos ^. from v2tuple)
    $ scale size size pic

