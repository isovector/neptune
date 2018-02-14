{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Scripting where

import Foreign.Lua
import Foreign.Lua.Api (newstate)
import Types
import Utils
import Orphans (unEnt)


globalGameState :: IORef GameState
globalGameState = unsafePerformIO
                . newIORef
                $ error "not initialized yet"
{-# NOINLINE globalGameState #-}


initLua :: IO LuaState
initLua = do
  l <- newstate
  _ <- runLuaWith l $ do
    openlibs

    registerHaskellFunction "hsNewEntity" $ liftGame $
      unEnt <$> newEntity defEntity

    exposeComponent "hsEntPos" pos
    exposeComponent "hsEntTalkColor" talkColor

    registerHaskellFunction "hsGetPlayer" . liftGame $ do
      player <- efor $ \e -> with isAvatar >> pure e
      pure . Optional $ listToMaybe player

    registerHaskellFunction "hsSay" $ liftGame .:. timedText

    _ <- dostring "package.path = package.path .. ';./scripts/?.lua'"
    liftIO . print =<< dostring "require 'init'"
  pure l


liftGame :: Game a -> Lua a
liftGame g = liftIO $ do
  s <- readIORef globalGameState
  (s', a) <- runGame s g
  writeIORef globalGameState s'
  pure a


exposeComponent :: ToLuaStack a => String -> (EntWorld 'FieldOf -> Maybe a) -> Lua ()
exposeComponent nm f =
  registerHaskellFunction nm $ \e -> liftGame $ do
    ent <- getEntity $ Ent e
    pure . Optional $ f ent


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

