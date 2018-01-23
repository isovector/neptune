{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE TemplateHaskell            #-}

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

import           Data.Default
import           BasePrelude hiding ((&), trace, rotate, resolution, Down, loop)
import           Codec.Picture
import           Control.Lens hiding (index, lazy, uncons)
import           Control.Monad.Coroutine
import           Control.Monad.Coroutine.SuspensionFunctors hiding (Reader)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Reader
import           Control.Monad.Writer (Writer, tell)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Debug.Trace as DT
import           Graphics.Gloss hiding (line)
import           Graphics.Gloss.Game (KeyState (..))
import           Linear.V2
import           Linear.Vector
import           Data.Function.Pointless


showTrace :: Show a => a -> a
showTrace = DT.trace =<< show


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
newtype ViewPos = ViewPos { unViewPos :: Pos } deriving (Generic, Num, Show, Eq, Ord)
instance Wrapped ViewPos

newtype Viewport = Viewport { unViewport :: Pos } deriving (Generic, Num, Show, Eq, Ord)
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
  , _avatar        :: SomeActor
  , _clock         :: Time
  , _rooms         :: !(Map Rooms SomeRoom)
  , _currentRoomId :: !Rooms
  , _activeHotspot :: Maybe Hotspot
  , _nextVerb      :: Maybe Verb
  }

instance Default System where
  def                  = System
      { _mousePos      = ViewPos zero
      , _mouseState    = Up
      , _cameraPos     = ViewPos zero
      , _viewportSize  = Viewport zero
      , _avatar        = SomeActor undefined
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
data Actor s = Actor
  { _actorPos'       :: Pos
  , _motion'         :: Maybe Motion
  , _onActorInteract :: Maybe (Actor s -> Verb -> Game (Actor s))
  , _onActorTick     :: Time -> Actor s -> Game (Actor s)
  , _actorState      :: s
  , _actorZIndex     :: Int
  , _actorGraphics'  :: Picture
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
-- | Existentialized 'Actor'.
data SomeActor where
  SomeActor :: { unSomeActor :: Actor s } -> SomeActor


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
type Machine = Coroutine (Request Pos Float) (ReaderT (Pos, SomeRoom) (Writer [GameAction]))


------------------------------------------------------------------------------
-- | Existential around 'Room' allowing us to pack them together.
data SomeRoom where
  SomeRoom :: HasRoom r => !(Room r) -> SomeRoom


------------------------------------------------------------------------------
-- | Datakind describing which rooms we can visit.
data Rooms
  = Study
  | City
  | CostumeShop
  deriving (Eq, Ord)


------------------------------------------------------------------------------
-- | State of a room.
data Room r = Room
  { _actors'    :: [SomeActor]
  , _model      :: RoomModel r
  , _layers'    :: !Picture
  , _size'      :: V2 Int
  , _navmesh'   :: !NavMesh
  , _hotspots'  :: Pos -> Maybe Hotspot
  , _roomScale' :: Float
  }


------------------------------------------------------------------------------
-- | Class describing a room.
class HasRoom (r :: Rooms) where
  type RoomModel r
  onRoomTick :: Time -> Room r -> Game (Room r)
  onRoomTick = const pure

  onRoomEnter :: Room r -> Game (Room r)
  onRoomEnter = pure

  onRoomLeave :: Room r -> Game (Room r)
  onRoomLeave = pure


------------------------------------------------------------------------------

makeLenses ''Actor
makeLenses ''Hotspot
makeLenses ''Room
makeLenses ''System


------------------------------------------------------------------------------
-- | An existentially quantified lens.
--
-- @ExLens' s a = Lens' (forall x. s x) a@
type ExLens' (s :: k -> *) a =
  forall f. Functor f => (a -> f a) -> (forall x. (s x) -> f (s x))


------------------------------------------------------------------------------
-- | Lift a 'Lens (Actor s) a' to a 'ExLens s a'.
liftOverSomeActor :: ExLens' Actor a
                  -> Lens' SomeActor a
liftOverSomeActor l =
  lens (\(SomeActor actor) -> actor ^. l)
       (\(SomeActor actor) -> SomeActor . flip (set l) actor)

actorPos :: Lens' SomeActor Pos
actorPos = liftOverSomeActor actorPos'

actorMotion :: Lens' SomeActor (Maybe Motion)
actorMotion = liftOverSomeActor motion'

actorGraphics :: Lens' SomeActor Picture
actorGraphics = liftOverSomeActor actorGraphics'

actorZ :: Lens' SomeActor Int
actorZ = liftOverSomeActor actorZIndex


------------------------------------------------------------------------------
-- | Run movement on the avatar.
avatarMotion :: Machine () -> Game ()
avatarMotion m = modifySystem $ avatar . actorMotion ?~ motion m

motion :: Machine () -> Motion
motion = Motion . const


------------------------------------------------------------------------------
-- | Lift a 'Lens (Actor s) a' to a 'ExLens s a'.
liftOverSomeRoom :: ExLens' Room a
                 -> Lens' SomeRoom a
liftOverSomeRoom l =
  lens (\(SomeRoom room) -> room ^. l)
       (\(SomeRoom room) -> SomeRoom . flip (set l) room)

actors :: Lens' SomeRoom [SomeActor]
actors = liftOverSomeRoom actors'

roomSize :: Lens' SomeRoom (V2 Int)
roomSize = liftOverSomeRoom size'

layers :: Lens' SomeRoom Picture
layers = liftOverSomeRoom layers'

navmesh :: Lens' SomeRoom NavMesh
navmesh = liftOverSomeRoom navmesh'

hotspots :: Lens' SomeRoom (Pos -> Maybe Hotspot)
hotspots = liftOverSomeRoom hotspots'

roomScale :: Lens' SomeRoom Float
roomScale = liftOverSomeRoom roomScale'


------------------------------------------------------------------------------
-- | Get the current room out of the system.
currentRoom :: Lens' System SomeRoom
currentRoom =
  lens (\s    -> s ^. rooms . at (s ^. currentRoomId) . to fromJust)
       (\s sr -> s &  rooms . at (s ^. currentRoomId) ?~ sr)

