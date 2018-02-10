{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Scripting where

import Control.Concurrent.MVar
import Foreign.Lua
import Foreign.Lua.Api (newstate)
import Types
import Utils

class Sorcery a where
  type SorceryT a :: *
  wizard :: a -> SorceryT a

instance Sorcery (Game ()) where
  type SorceryT (Game ()) = Lua ()
  wizard = liftIO . asyncGame

-- instance Sorcery a => Sorcery (Pos -> a) where
--   type SorceryT (Pos -> a) = (LuaNumber, LuaNumber) -> SorceryT a
--   wizard (V2 x y) = wizard @a (V2 x y)



delayedActions :: MVar [Game ()]
delayedActions = unsafePerformIO newEmptyMVar
{-# NOINLINE delayedActions #-}

asyncGame :: Game () -> IO ()
asyncGame g = do
  success <- tryPutMVar delayedActions [g]
  when (not success) .
    modifyMVar_ delayedActions $ pure . (g:)

unLuaNumber :: LuaNumber -> Double
unLuaNumber (LuaNumber d) = d

initLua :: IO LuaState
initLua = do
  l <- newstate
  _ <- runLuaWith l $ do
    openlibs
    registerHaskellFunction "hsSay" $
      \(unLuaNumber -> r)
       (unLuaNumber -> g)
       (unLuaNumber -> b)
       (unLuaNumber -> x)
       (unLuaNumber -> y) s -> liftIO @Lua . asyncGame $ timedText (rgb r g b) (V2 x y) s

    _ <- dostring "package.path = package.path .. ';./scripts/?.lua'"
    liftIO . print =<< dostring "require 'init'"
    liftIO . print =<< dostring "require 'test'"
  pure l

liftLua :: Lua a -> Game ()
liftLua lua = do
  l <- getGlobals _gLuaState
  void . liftIO $ runLuaWith l lua

updateLua :: Game ()
updateLua = do
  liftLua $ dostring "tasks:continue()"
  void $ liftIO (tryTakeMVar delayedActions) >>= \case
    Just as -> sequence as
    Nothing -> pure []

asyncLua :: String -> Game ()
asyncLua str = liftLua
             . dostring
             $ "tasks:start(function() " <> str <> " end)"

