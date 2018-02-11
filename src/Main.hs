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
    , _viewport      = viewPortInit
    , _timers        = M.empty
    , _gInputDFA     = IStart
    , _gLuaState     = l
    , _gLuaActions   = []
    , _gController   = Controller
                         (const False)
                         (const False)
    }


main :: IO ()
main = do
  engine <- startup config
  g      <- defGlobals
  start  <- realToFrac <$> getPOSIXTime
  s0     <- execGame (g, (0, defWorld)) $ initialize

  flip fix (start, s0) $ \loop (last, s) -> do
    now <- getNow
    s' <- execGame s $ do
      update
      tick $ now - last

      scene <- draw
      liftIO $ render engine scene (640, 480)

    s'' <- updateLua s'
    shouldQuit <- SDL.quitRequested
    unless shouldQuit $ loop (now, s'')

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
coinPic = move (V2 (-72) (-28)) $ unsafeLoadPng $ "assets" </> "actionbar"

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

