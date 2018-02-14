{-# LANGUAGE NoImplicitPrelude #-}

module Utils where

import Types
import Game.Sequoia.Text
import Game.Sequoia.Color
import Data.Text (pack)
import Timers

renderText :: Color -> String -> Form
renderText c str = toForm
                 . text
                 . height 12
                 . color white
                 . stroke (defaultLine { lineColor = c, lineWidth = 4 } )
                 . toText
                 $ pack str

timedText :: Color -> Pos -> String -> Game ()
timedText c p str = do
  let time = (* 0.5)
           . fromIntegral
           . max 5
           . length
           $ words str
  ent <- textEnt c p str
  startTimer (TimerText ent) time $ do
    setEntity ent defEntity'
      { pos = Unset
      , gfx = Unset
      }


textEnt :: Color -> Pos -> String -> Game Ent
textEnt c p str =
  newEntity defEntity
    { pos = Just p
    , gfx = Just $ renderText c str
    }

