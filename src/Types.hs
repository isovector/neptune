{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module Types
  ( module Types
  , module BasePrelude
  , module Control.Lens
  , module Linear.V2
  , module Linear.Vector
  , module Graphics.Gloss
  , module Data.Ecstasy
  , KeyState (..)
  , Image
  , PixelRGBA8
  , ask
  , asks
  , lift
  , liftIO
  , module Data.Function.Pointless
  , module Graphics.Gloss.Data.ViewPort
  ) where

import           BasePrelude hiding ((&), trace, rotate, resolution, Down, loop)
import           Codec.Picture
import           Control.Lens hiding (index, lazy, uncons, without)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State (StateT (..), gets, modify)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Reader
import           Data.Ecstasy
import           Data.Function.Pointless
import           Data.Map.Strict (Map)
import qualified Debug.Trace as DT
import           Graphics.Gloss hiding (line)
import           Graphics.Gloss.Data.ViewPort
import           Graphics.Gloss.Game (KeyState (..))
import           Linear.V2
import           Linear.Vector


showTrace :: Show a => a -> a
showTrace = DT.trace =<< show

data NavTarget
  = NavTo Pos
  | Following [Pos]
  deriving (Show)

data EntWorld f = Entity
  { pos      :: Component f 'Field Pos
  , pathing  :: Component f 'Field NavTarget
  , speed    :: Component f 'Field Float
  , gfx      :: Component f 'Field Picture
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
type Time = Float
type Pos = V2 Float


------------------------------------------------------------------------------
-- | Iso witness between tuples and V2s.
v2tuple :: Iso' (a, a) (V2 a)
v2tuple = iso (uncurry V2) $ \(V2 x y) -> (x, y)


------------------------------------------------------------------------------
-- | Core engine state.
data Globals = Globals
  { _mousePos      :: Pos
  , _mouseState    :: KeyState
  , _viewport      :: ViewPort
  , _rooms         :: !(Map Rooms Room)
  , _currentRoomId :: !Rooms
  , _nextVerb      :: Maybe Verb
  , _timers        :: !(Map TimerType Timer)
  }

instance Show Globals where
  show _ = "Globals"


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
  = Study
  | City
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
  { _layers    :: !Picture
  , _size'     :: !(V2 Int)
  , _navmesh   :: !NavMesh
  , _roomScale :: !Float
  , _hotspots  :: Pos -> Maybe Hotspot
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

