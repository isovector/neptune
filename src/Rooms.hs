module Rooms
  ( module Rooms
  , module Rooms.City
  , module Rooms.Study
  , module Rooms.CostumeShop
  ) where

import Rooms.City
import Rooms.CostumeShop
import Rooms.Study
import Types


------------------------------------------------------------------------------
-- | Run the existentialized 'Room' tick.
tickRoom :: Time -> SomeRoom -> Game SomeRoom
tickRoom dt (SomeRoom room) = SomeRoom <$> onRoomTick dt room

