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
tickRoom :: Time -> Room -> Game Room
tickRoom dt room = onRoomTick room dt room

