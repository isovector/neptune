{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Data.Ecstasy hiding (System)
import qualified Data.Map as M
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game (playIO)
import           LoadImage
import           Rooms
import           System
import           System.FilePath.Posix
import           Tick
import           Types
import           Viewport


screen :: Display
screen = InWindow "Neptune" (640, 480) (0, 0)


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
  , _timers        = M.empty
  , _gInputDFA     = IStart
  }


main :: IO ()
main = do
  s' <- execGame (defGlobals, (0, defWorld)) $ do
          void $ newEntity $ defEntity
            { pos = Just $ V2 135 176
            , gfx = Just
                  . translate 0 15
                  . color (makeColor 0 0 1 1)
                  $ circleSolid 15
            , speed = Just 50
            , hasFocus = Just ()
            , isAvatar = Just ()
            }

  playIO screen
         black
         60
         s'
         drawGame
         update
         tick

coinPic :: Picture
coinPic = unsafeLoadPng $ "assets" </> "actionbar"

drawGame :: GameState -> IO Picture
drawGame ms = evalGame ms $ do
  room <- getGlobals $ view currentRoom
  coin <- getGlobals $ view gInputDFA

  vp   <- getViewport
  gfxs <- efor . const $
    (,) <$> get pos
        <*> get gfx

  let pic  = getRoomPicture room
      size = room ^. roomScale

  pure . applyViewPortToPicture vp
       . pictures
       $ [pic]
      ++ fmap (uncurry $ drawGfx size) gfxs
      ++ case coin of
           ICoinOpen pos _ -> [translate' pos coinPic]
           _ -> []

translate' :: Pos -> Picture -> Picture
translate' pos =
  uncurry translate (toDrawCoord pos ^. from v2tuple)

drawGfx :: Float -> Pos -> Picture -> Picture
drawGfx size worldPos =
  translate' worldPos . scale size size

