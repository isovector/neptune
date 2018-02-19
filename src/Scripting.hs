{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Scripting where

import Foreign.Lua
import Foreign.Lua.Api (newstate)
import Orphans ()
import Types
import Utils


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

    exposeComponent "pos"       pos       $ \a s -> s { pos       = Set a }
    exposeComponent "speed"     speed     $ \a s -> s { speed     = Set a }
    exposeComponent "talkColor" talkColor $ \a s -> s { talkColor = Set a }
    exposeComponent "fromRoom"  fromRoom  $ \a s -> s { fromRoom  = Set a }
    exposeUnique    "isAvatar"  isAvatar  $ \a s -> s { isAvatar  = a }
    exposeUnique    "hasFocus"  hasFocus  $ \a s -> s { hasFocus  = a }

    registerHaskellFunction "hsGetPlayer" . liftGame $ do
      player <- efor $ \e -> with isAvatar >> pure e
      pure . Optional $ listToMaybe player

    registerHaskellFunction "hsMakeRedDude" $ \e -> liftGame $ do
      setEntity e $ defEntity'
        { gfx = Set $ filled (rgb 1 0 0) $ rect 30 30
        }

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

exposeUnique
    :: String
    -> (EntWorld 'FieldOf -> Maybe ())
    -> (Update () -> EntWorld 'SetterOf -> EntWorld 'SetterOf)
    -> Lua ()
exposeUnique nm fGet fSet =
  exposeComponent nm
      (maybe (Just False)
             (const $ Just True)
        . fGet)
      $ \a s -> fSet (bool Unset (Set ()) a) s


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


loadRoom :: String -> Game ()
loadRoom room = do
  emap $ do
    with fromRoom
    pure delEntity

  asyncLua $ mconcat
    [ "require 'rooms.", room, "'"
    , "\n"
    , "rooms['", room, "']:load()"
    ]

