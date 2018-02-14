{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans where

import Data.ByteString (ByteString)
import Foreign.Lua
import Types
import Game.Sequoia.Color (Color (..))

unEnt :: Ent -> Int
unEnt (Ent e) = e


instance FromLuaStack Int where
  peek si = do
    LuaInteger d <- peek si
    pure $ fromIntegral d

instance ToLuaStack Int where
  push = push . LuaInteger . fromIntegral

instance FromLuaStack Double where
  peek si = do
    LuaNumber d <- peek si
    pure d

instance ToLuaStack Double where
  push = push . LuaNumber

instance FromLuaStack Ent where
  peek si = Ent <$> getField si "ent"

instance ToLuaStack Ent where
  push (Ent e) = callAndKeep "Actor" e


instance FromLuaStack Color where
  peek si =
    rgba <$> getField si "r"
         <*> getField si "g"
         <*> getField si "b"
         <*> getField si "a"

instance ToLuaStack Color where
  push (Color r g b a) =
    callAndKeep "Color" r g b a


instance FromLuaStack Pos where
  peek si =
    V2 <$> getField si "x"
       <*> getField si "y"

getField
    :: FromLuaStack a
    => StackIndex
    -> ByteString
    -> Lua a
getField si str = do
  pushstring str
  gettable si
  x <- peek stackTop
  pop 1
  pure x


class LuaCallAndKeep a where
  callAndKeep' :: String -> Lua () -> NumArgs -> a

instance LuaCallAndKeep (Lua ()) where
  callAndKeep' fnName pushArgs nargs = do
    getglobal' fnName
    pushArgs
    call nargs 1

instance (ToLuaStack a, LuaCallAndKeep b) => LuaCallAndKeep (a -> b) where
  callAndKeep' fnName pushArgs nargs x =
    callAndKeep' fnName (pushArgs *> push x) (nargs + 1)

callAndKeep :: LuaCallAndKeep a => String -> a
callAndKeep s = callAndKeep' s (pure ()) 0


instance ToLuaStack Pos where
  push (V2 x y) = callAndKeep "V2" x y

