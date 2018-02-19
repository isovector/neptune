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
      newEntity defEntity

    exposeComponent "pos"       pos       $ \a s -> s { pos = Set a }
    exposeComponent "talkColor" talkColor $ \a s -> s { talkColor = Set a }

    registerHaskellFunction "hsGetPlayer" . liftGame $ do
      player <- efor $ \e -> with isAvatar >> pure e
      pure . Optional $ listToMaybe player

    registerHaskellFunction "hsSay" $ liftGame .:. timedText
    registerHaskellFunction "hsWalkTo" $ \e p -> liftGame $ do
      setEntity e $ defEntity'
        { pathing = Set $ NavTo p
        }

    _ <- dostring "package.path = package.path .. ';./scripts/?.lua'"
    liftIO . print =<< dostring "require 'init'"
  pure l


liftGame :: Game a -> Lua a
liftGame g = liftIO $ do
  s <- readIORef globalGameState
  (s', a) <- runGame s g
  writeIORef globalGameState s'
  pure a


exposeComponent
    :: ( ToLuaStack a
       , FromLuaStack a
       )
    => String
    -> (EntWorld 'FieldOf -> Maybe a)
    -> (a -> EntWorld 'SetterOf -> EntWorld 'SetterOf)
    -> Lua ()
exposeComponent nm fGet fSet = do
  registerHaskellFunction ("hsGet" <> nm') $ \e -> liftGame $ do
    ent <- getEntity e
    pure . Optional $ fGet ent
  registerHaskellFunction ("hsSet" <> nm') $ \e a -> liftGame $ do
    setEntity e $ fSet a defEntity'
  where
    nm' = nm & _head %~ toUpper


------------------------------------------------------------------------------
-- | Not re-entrant. Don't event try!
updateLua :: Time -> GameState -> IO GameState
updateLua dt s@(g, _) = do
  writeIORef globalGameState s
  x <- runLuaWith (_gLuaState g) $ do
         void . sequence $ _gLuaActions g
         dostring $ "tasks:continue(" <> show dt <> ")"
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

