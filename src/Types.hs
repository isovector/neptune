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
  , KeyState (..)
  , Image
  , PixelRGBA8
  , ask
  , asks
  , tell
  , lift
  , module Data.Default
  , module Data.Function.Pointless
  ) where

import           BasePrelude hiding ((&), trace, rotate, resolution, Down, loop)
import           Codec.Picture
import           Control.Lens hiding (index, lazy, uncons)
import           Control.Monad.Coroutine
import           Control.Monad.Coroutine.SuspensionFunctors hiding (Reader)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Reader
import           Control.Monad.Writer (Writer, tell)
import           Control.Monad.State (StateT (..))
import           Data.Default
import qualified Data.Ecstasy as E
import           Data.Ecstasy hiding (System)
import           Data.Function.Pointless
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Debug.Trace as DT
import           Graphics.Gloss hiding (line)
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
  , gfx      :: Component f 'Field Picture
  , isAvatar :: Component f 'Unique ()
  } deriving (Generic)

deriving instance Show (EntWorld 'WorldOf)

type Game' = E.SystemT EntWorld (StateT System IO)
type MyState = (System, SystemState EntWorld)

runGame'
    :: MyState
    -> Game' a
    -> IO (MyState, a)
runGame' (s, w) m = do
  ((w', a), s') <- flip runStateT s
                 $ yieldSystemT w m
  pure ((s', w'), a)


execGame'
    :: MyState
    -> Game' a
    -> IO (MyState)
execGame' = (fmap fst .) . runGame'

evalGame'
    :: MyState
    -> Game' a
    -> IO a
evalGame' = (fmap snd .) . runGame'


------------------------------------------------------------------------------
type Time = Float
type Pos = V2 Int

type Game = ReaderT System (Writer [GameAction])


------------------------------------------------------------------------------
-- | "Global" actions which can be run in any 'Game' context on the next frame.
data GameAction
  = Trace String
  | ModifySystem (System -> System)
  | ChangeRoom Rooms Pos

trace :: String -> Game ()
trace = tell . pure . Trace

modifySystem :: (System -> System) -> Game ()
modifySystem = tell . pure . ModifySystem

changeRoom :: Rooms -> Pos -> Game ()
changeRoom = tell . pure .: ChangeRoom


------------------------------------------------------------------------------
-- | A position in view space.
newtype ViewPos = ViewPos
  { unViewPos :: Pos
  } deriving (Generic, Num, Show, Eq, Ord)
instance Wrapped ViewPos

newtype Viewport = Viewport
  { unViewport :: Pos
  } deriving (Generic, Num, Show, Eq, Ord)
instance Wrapped Viewport


------------------------------------------------------------------------------
-- | Iso witness between tuples and V2s.
v2tuple :: Iso' (a, a) (V2 a)
v2tuple = iso (uncurry V2) $ \(V2 x y) -> (x, y)


------------------------------------------------------------------------------
-- | Core engine state.
data System = System
  { _mousePos      :: ViewPos
  , _mouseState    :: KeyState
  , _cameraPos     :: ViewPos
  , _viewportSize  :: Viewport
  , _avatar        :: Actor
  , _clock         :: Time
  , _rooms         :: !(Map Rooms Room)
  , _currentRoomId :: !Rooms
  , _activeHotspot :: Maybe Hotspot
  , _nextVerb      :: Maybe Verb
  }

instance Show System where
  show _ = "System"

instance Default System where
  def                  = System
      { _mousePos      = ViewPos zero
      , _mouseState    = Up
      , _cameraPos     = ViewPos zero
      , _viewportSize  = Viewport zero
      , _avatar        = undefined
      , _clock         = 0
      , _rooms         = M.empty
      , _currentRoomId = Study
      , _activeHotspot = Nothing
      , _nextVerb      = Nothing
      }


------------------------------------------------------------------------------
-- | Game-specific state.
data GameState = GameState


------------------------------------------------------------------------------
-- | Actor with internal state 's'.
data Actor = Actor
  { _actorPos       :: Pos
  , _motion'         :: Maybe Motion
  , _onActorInteract :: Maybe (Actor -> Verb -> Game Actor)
  , _onActorTick     :: Time -> Actor -> Game Actor
  , _actorZ     :: Int
  , _actorGraphics  :: Picture
  }


------------------------------------------------------------------------------
-- | A differentiated region in the background.
data Hotspot = Hotspot
  { _onHotspotInteract  :: Maybe (Verb -> Game ())
  , _onHotspotEnter     :: Game ()
  , _onHotspotLeave     :: Game ()
  , _hotspotId          :: Word8
  , _hotspotDefaultVerb :: Verb
  }

instance Eq Hotspot where
  (==) = (==) `on `_hotspotId

instance Default Hotspot where
  def = Hotspot
      { _onHotspotInteract  = Nothing
      , _onHotspotEnter     = pure ()
      , _onHotspotLeave     = pure ()
      , _hotspotId          = 0
      , _hotspotDefaultVerb = Examine
      }


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

newtype Motion = Motion (Float -> Machine ())
type Machine = Coroutine (Request Pos Float) (ReaderT (Pos, Room) (Writer [GameAction]))



------------------------------------------------------------------------------
-- | Datakind describing which rooms we can visit.
data Rooms
  = Study
  | City
  | CostumeShop
  deriving (Eq, Ord)


------------------------------------------------------------------------------
-- | State of a room.
data Room = Room
  { _actors    :: [Actor]
  , _layers    :: !Picture
  , _size'      :: V2 Int
  , _navmesh   :: !NavMesh
  , _hotspots  :: Pos -> Maybe Hotspot
  , _roomScale :: Float
  }

------------------------------------------------------------------------------

makeLenses ''Actor
makeLenses ''Hotspot
makeLenses ''Room
makeLenses ''System


actorMotion :: Lens' Actor (Maybe Motion)
actorMotion = motion'


------------------------------------------------------------------------------
-- | Run movement on the avatar.
avatarMotion :: Machine () -> Game ()
avatarMotion m = modifySystem $ avatar . actorMotion ?~ motion m

motion :: Machine () -> Motion
motion = Motion . const

roomSize :: Lens' Room (V2 Int)
roomSize = size'


------------------------------------------------------------------------------
-- | Get the current room out of the system.
currentRoom :: Lens' System Room
currentRoom =
  lens (\s    -> s ^. rooms . at (s ^. currentRoomId) . to fromJust)
       (\s sr -> s &  rooms . at (s ^. currentRoomId) ?~ sr)

