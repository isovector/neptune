{-# LANGUAGE DataKinds                       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving      #-}
{-# LANGUAGE NoImplicitPrelude               #-}
{-# LANGUAGE PolyKinds                       #-}
{-# LANGUAGE RecordWildCards                 #-}
{-# LANGUAGE TemplateHaskell                 #-}
{-# LANGUAGE TupleSections                   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Types
  ( module Types
  , module BasePrelude
  , module Control.Lens
  , module Linear.V2
  , module Linear.Vector
  , module Data.Ecstasy
  , Image
  , PixelRGBA8
  , ask
  , asks
  , lift
  , MonadIO (..)
  , module Data.Function.Pointless
  , module Game.Sequoia
  , showTrace
  , MouseButton (..)
  ) where

-- import           Game.Sequoia.Keyboard (Key (..))
import           BasePrelude hiding ((&), trace, rotate, resolution, Down, loop, group)
import           Codec.Picture
import qualified Constants as CD
import           Control.Lens hiding (index, lazy, uncons, without)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.State (StateT (..), gets, modify)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Reader
import           Data.Ecstasy
import           Data.Function.Pointless
import           Data.Map.Strict (Map)
import           Foreign.Lua (LuaState)
import           Foreign.Marshal.Alloc (alloca)
import           Game.Sequoia hiding (render, step, V2, E)
import           Game.Sequoia.Utils (showTrace)
import           Linear.V2
import           Linear.Vector
import           SDL.Input.Mouse (MouseButton (..), getMouseButtons)
import qualified SDL.Raw as SDL


CD.Constants {..} = read $ unsafePerformIO $ readFile "assets/constants"

data BB = BB
  { leftX   :: Double
  , rightX  :: Double
  , topY    :: Double
  , bottomY :: Double
  } deriving (Eq, Ord, Show)


rectBB :: Double -> Double -> BB
rectBB ((/2) -> x) ((/2) -> y) =
  BB (-x) x (-y) y


moveBB :: Pos -> BB -> BB
moveBB (V2 x y) BB{..} = BB
  { leftX   = leftX   + x
  , rightX  = rightX  + x
  , topY    = topY    + y
  , bottomY = bottomY + y
  }


inBB :: BB -> Pos -> Bool
inBB BB{..} (V2 x y) = and
  [ x >= leftX
  , x <  rightX
  , y >= topY
  , y <  bottomY
  ]


data BBSurface a = BBSurface [(BB, a)]
  deriving (Eq, Ord, Show)

getBBSurface :: BBSurface a -> Pos -> Maybe a
getBBSurface (BBSurface bs) p =
  getFirst . flip foldMap bs $ \(b, a) ->
    if inBB b p
       then First $ Just a
       else First $ Nothing


data NavTarget
  = NavTo Pos
  | Following [Pos]
  deriving (Show)

data EntWorld f = Entity
  { pos      :: Component f 'Field Pos
  , pathing  :: Component f 'Field NavTarget
  , speed    :: Component f 'Field Double
  , gfx      :: Component f 'Field Form
  , isAvatar :: Component f 'Unique ()
  , hasFocus :: Component f 'Unique ()
  } deriving (Generic)

deriving instance Show (EntWorld 'WorldOf)

type Game = SystemT EntWorld (StateT Globals IO)
type GameState = (Globals, SystemState EntWorld)

runGame
    :: GameState
    -> Game a
    -> IO (GameState, a)
runGame (s, w) m = do
  ((w', a), s') <- flip runStateT s
                 $ yieldSystemT w m
  pure ((s', w'), a)


execGame
    :: GameState
    -> Game a
    -> IO GameState
execGame = (fmap fst .) . runGame

evalGame
    :: GameState
    -> Game a
    -> IO a
evalGame = (fmap snd .) . runGame


------------------------------------------------------------------------------
type Pos = V2 Double


------------------------------------------------------------------------------
-- | Iso witness between tuples and V2s.
v2tuple :: Iso' (a, a) (V2 a)
v2tuple = iso (uncurry V2) $ \(V2 x y) -> (x, y)


------------------------------------------------------------------------------
-- | Core engine state.
data Globals = Globals
  { _viewport      :: !ViewPort
  , _rooms         :: !(Map Rooms Room)
  , _currentRoomId :: !Rooms
  , _timers        :: !(Map TimerType Timer)
  , _gInputDFA     :: !InputDFA
  , _gLuaState     :: !LuaState
  , _gController   :: !Controller
  }

instance Show Globals where
  show _ = "Globals"


data Controller = Controller
  { _ctrlMouse :: MouseButton -> Bool
  }

instance Show Controller where
  show _ = "Controller"

getController :: Game Controller
getController =
  Controller <$> liftIO getMouseButtons


getMousePos :: Game Pos
getMousePos = do
  liftIO $
    alloca $ \xptr ->
    alloca $ \yptr -> do
      _ <- SDL.getMouseState xptr yptr
      x <- peek xptr
      y <- peek yptr
      pure $ V2 (fromIntegral x) (fromIntegral y)


getGlobals :: (Globals -> a) -> Game a
getGlobals = lift . gets

setGlobals :: (Globals -> Globals) -> Game ()
setGlobals = lift . modify

------------------------------------------------------------------------------
-- | Get the size of an image.
imageSize :: Image a -> V2 Int
imageSize img = V2 (imageWidth img) (imageHeight img)


------------------------------------------------------------------------------
-- | Clamp a vector.
clamp :: Ord a => V2 a -> V2 a -> V2 a -> V2 a
clamp (V2 lx ly) (V2 ux uy) (V2 x y) = V2 (clamp' lx ux x) (clamp' ly uy y)

clamp' :: Ord a => a -> a -> a -> a
clamp' l u z = min u $ max l z


data Timer = Timer
  { _tTime     :: Time
  , _tCallback :: Game ()
  }

data TimerType
  = TimerCoin
  deriving (Eq, Ord)

------------------------------------------------------------------------------
-- | Navigation mesh.
data NavMesh = NavMesh
  { navigate   :: !(Pos -> Pos -> Maybe [Pos])
  , isWalkable :: !(Pos -> Bool)
  }


------------------------------------------------------------------------------
-- TODO(sandy):
data Verb
  = Examine
  | TalkTo
  | Touch
  deriving (Eq, Show, Ord)

data Item = Item
  deriving (Eq, Show, Ord)

data InteractionTarget
  = InteractionHotspot Hotspot
  | InteractionActor Ent


------------------------------------------------------------------------------
-- | Datakind describing which rooms we can visit.
data Rooms
  = City
  | CostumeShop
  deriving (Eq, Ord)


------------------------------------------------------------------------------
-- | Hotspots
data Hotspot = Hotspot
  { _hsId          :: Word8
  , _hsName        :: String
  , _hsDefaultVerb :: Verb
  }

------------------------------------------------------------------------------
-- | State of a room.
data Room = Room
  { _layers    :: !Form
  , _size'     :: !(V2 Int)
  , _navmesh   :: !NavMesh
  , _roomScale :: !Double
  , _hotspots  :: Pos -> Maybe Hotspot
  }

data InputDFA
  = IStart
  | IBeforeCoin
  | ICoinOpen Pos InteractionTarget


data ViewPort = ViewPort
  { viewPortTranslate :: !(V2 Double)
  , viewPortRotate  :: !Double
  , viewPortScale   :: !Double
  }

viewPortInit :: ViewPort
viewPortInit = ViewPort
  { viewPortTranslate = zero
  , viewPortRotate  = 0
  , viewPortScale   = 1
  }




------------------------------------------------------------------------------

makeLenses ''Room
makeLenses ''Globals
makeLenses ''Hotspot
makeLenses ''Timer


roomSize :: Lens' Room (V2 Int)
roomSize = size'


------------------------------------------------------------------------------
-- | Get the current room out of the Globals.
currentRoom :: Lens' Globals Room
currentRoom =
  lens (\s    -> s ^. rooms . at (s ^. currentRoomId) . to fromJust)
       (\s sr -> s &  rooms . at (s ^. currentRoomId) ?~ sr)

