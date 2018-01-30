{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Scripting where

import Foreign.Lua
import Foreign.Lua.Api (newstate)
import Types

initLua :: IO LuaState
initLua = do
  l <- newstate
  _ <- runLuaWith l $ do
    openlibs
    dostring "package.path = package.path .. ';./scripts/?.lua'"
    dostring "require 'tasks'"
    dostring "require 'test'"
  pure l

updateLua :: Game ()
updateLua = do
  l <- getGlobals _gLuaState
  void . liftIO . runLuaWith l $ dostring "tasks:continue()"

