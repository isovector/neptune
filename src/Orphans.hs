{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans where

import Foreign.Lua
import Types
import Game.Sequoia.Color (Color (..))


unLuaNumber :: LuaNumber -> Double
unLuaNumber (LuaNumber d) = d


instance FromLuaStack Ent where
  peek si = do
    LuaInteger e <- peek si
    pure . Ent $ fromIntegral e

instance ToLuaStack Ent where
  push (Ent e) =
    push . LuaInteger $ fromIntegral e

instance FromLuaStack Color where
  peek si = do
    x@(_, _, _) <- peek si
    let (r, g, b) = x & each %~ unLuaNumber
    pure $ rgb r g b

instance ToLuaStack Color where
  push (Color r g b _) =
    push ( LuaNumber r
         , LuaNumber g
         , LuaNumber b
         )


instance FromLuaStack Pos where
  peek si = do
    z@(_, _) <- peek si
    let (x, y) = z & each %~ unLuaNumber
    pure $ V2 x y

instance ToLuaStack Pos where
  push (V2 x y) =
    push ( LuaNumber x
         , LuaNumber y
         )

