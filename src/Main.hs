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
import Scripting


screen :: Display
screen = InWindow "Neptune" (640, 480) (0, 0)


defGlobals :: IO Globals
defGlobals = do
  l <- initLua
  pure $ Globals
    { _rooms = [ (Study,       studyRoom)
              , (City,        cityRoom)
              , (CostumeShop, costumeRoom)
              ]
    , _currentRoomId = CostumeShop
    , _mousePos      = zero
    , _mouseState    = Up
    , _viewport      = viewPortInit
    , _timers        = M.empty
    , _gInputDFA     = IStart
    , _gLuaState     = l
    }


main :: IO ()
main = do
  g <- defGlobals
  s' <- execGame (g, (0, defWorld)) $ do
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
           ICoinOpen p _ -> [translate' p coinPic]
           _ -> []

translate' :: Pos -> Picture -> Picture
translate' = uncurry translate
           . view (from v2tuple)
           . toDrawCoord

drawGfx :: Float -> Pos -> Picture -> Picture
drawGfx size worldPos =
  translate' worldPos . scale size size

