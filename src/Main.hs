{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Data.Ecstasy hiding (System)
import qualified Data.Map as M
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Game.Sequoia (startup, render, EngineConfig (..))
import           LoadImage
import           Rooms
import qualified SDL.Raw as SDL
import           Scripting
import           System
import           System.FilePath.Posix
import           Tick
import           Types
import           Viewport


getNow :: MonadIO m => m Double
getNow = liftIO $ realToFrac <$> getPOSIXTime


defGlobals :: IO Globals
defGlobals = do
  l <- initLua
  pure $ Globals
    { _rooms = [ (City,        cityRoom)
              ,  (CostumeShop, costumeRoom)
              ]
    , _currentRoomId = CostumeShop
    , _mousePos      = zero
    , _mouseState    = False
    , _viewport      = viewPortInit
    , _timers        = M.empty
    , _gInputDFA     = IStart
    , _gLuaState     = l
    }


main :: IO ()
main = do
  engine <- startup config
  g <- defGlobals
  start  <- realToFrac <$> getPOSIXTime

  evalGame (g, (0, defWorld)) $ do
    initialize
    flip fix start $ \loop last -> do
      now <- getNow
      tick $ now - last

      let elapsed = now - start
      scene <- draw
      liftIO $ render engine scene (640, 480)

      shouldQuit <- liftIO $ SDL.quitRequested
      unless shouldQuit $ loop now

  where
    config = EngineConfig
               (640, 480)
               "Neptune"
               $ rgb 0 0 0


initialize :: Game ()
initialize = do
  void $ newEntity $ defEntity
    { pos = Just $ V2 135 176
    , gfx = Just
          . filled (rgb 0 0 1)
          $ circle 15
    , speed = Just 50
    , hasFocus = Just ()
    , isAvatar = Just ()
    }

coinPic :: Form
coinPic = unsafeLoadPng $ "assets" </> "actionbar"

draw :: Game Element
draw = do
  room <- getGlobals $ view currentRoom
  coin <- getGlobals $ view gInputDFA

  -- vp   <- getViewport
  gfxs <- efor . const $
    (,) <$> get pos
        <*> get gfx

  let pic  = getRoomPicture room
      size = room ^. roomScale

  pure . collage 640 480
       . pure
       -- . applyViewPortToPicture vp
       . group
       $ [ pic ]
      ++ fmap (uncurry $ drawGfx size) gfxs
      ++ case coin of
           ICoinOpen p _ -> [translate' p coinPic]
           _ -> []

translate' :: Pos -> Form -> Form
translate' = move

drawGfx :: Double -> Pos -> Form -> Form
drawGfx size worldPos =
  translate' worldPos . scale size

