{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Scripting where

import Foreign.Lua
import Foreign.Lua.Api (newstate)
import Types
import Utils
import Orphans ()


globalGameState :: IORef GameState
globalGameState = unsafePerformIO $ newIORef undefined
{-# NOINLINE globalGameState #-}

initLua :: IO LuaState
initLua = do
  l <- newstate
  _ <- runLuaWith l $ do
    openlibs

    registerHaskellFunction "hsEntPos" $ \e -> liftGame $ do
       ent <- getEntity e
       pure . Optional $ pos ent
    registerHaskellFunction "hsGetPlayer" . liftGame $ do
      player <- efor $ \e -> with isAvatar >> pure e
      pure . Optional $ listToMaybe player
    registerHaskellFunction "hsSay" $ liftGame .:. timedText

    _ <- dostring "package.path = package.path .. ';./scripts/?.lua'"
    liftIO . print =<< dostring "require 'init'"
    liftIO . print =<< dostring "require 'test'"
  pure l


liftGame :: Game a -> Lua a
liftGame g = liftIO $ do
  s <- readIORef globalGameState
  (s', a) <- runGame s g
  writeIORef globalGameState s'
  pure a


------------------------------------------------------------------------------
-- | Not re-entrant. Don't event try!
updateLua :: GameState -> IO GameState
updateLua s@(g, _) = do
  writeIORef globalGameState s
  x <- runLuaWith (_gLuaState g) $ do
         void . sequence $ _gLuaActions g
         dostring "tasks:continue()"
  when (x /= OK) . liftIO $ print x

  s' <- readIORef globalGameState
  execGame s' . setGlobals
              $ gLuaActions .~ []

asyncLua :: String -> Game ()
asyncLua str = do
  let lua = void
          . dostring
          $ "tasks:start(function() " <> str <> " end)"
  setGlobals $ gLuaActions %~ (lua :)

