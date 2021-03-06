{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
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


gameWidth :: Int
gameWidth = 640

gameHeight :: Int
gameHeight = 480

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
  s0     <- execGame (g, (0, defWorld)) initialize

  flip fix (start, s0) $ \loop (prev, s) -> do
    now <- getNow
    let dt = now - prev
    s' <- execGame s $ do
      update
      tick dt

      scene <- draw
      liftIO $ render engine scene (640, 480)

    s'' <- updateLua dt s'
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
          . move (V2 0 (-15))
          . filled (rgb 0 0 1)
          $ circle 15
    , speed = Just 50
    , talkColor = Just $ rgb 0 0.7 1
    , hasFocus = Just ()
    , isAvatar = Just ()
    }

  void $ newEntity $ defEntity
    { pos = Just $ V2 254 197
    , gfx = Just
          . move (V2 (-44) $ -90)
          . unsafeLoadPng
          $ "assets" </> "costume" </> "table"
    }

  void $ newEntity $ defEntity
    { pos         = Just $ V2 (fromIntegral gameWidth / 2) (fromIntegral gameHeight - 40)
    , gfx         = Nothing
    , isNarration = Just ()
    }

  asyncLua "require 'test'"
  loadRoom "costume"

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

  pure . collage gameWidth gameHeight
       . pure
       -- . applyViewPortToPicture vp
       . group
       $ [pic]
      ++ ( fmap (uncurry $ drawGfx size)
         $ sortOn (view _y . fst) gfxs
         )
      ++ case coin of
           ICoinOpen p _ -> [translate' p coinPic]
           _ -> []

translate' :: Pos -> Form -> Form
translate' = move

drawGfx :: Double -> Pos -> Form -> Form
drawGfx size worldPos =
  translate' worldPos . scale size

