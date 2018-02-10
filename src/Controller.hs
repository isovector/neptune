{-# LANGUAGE NoImplicitPrelude #-}

module Controller where

import Types

new :: MouseButton -> Bool
new = const True

old :: MouseButton -> Bool
old = const False

risingEdge :: Controller -> Controller -> Bool
risingEdge = (\a b -> not a && b)
        `on` flip _ctrlMouse (ButtonExtra mouseLeft)

fallingEdge :: Controller -> Controller -> Bool
fallingEdge = (\a b -> a && not b)
         `on` flip _ctrlMouse (ButtonExtra mouseLeft)

