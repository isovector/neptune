{-# LANGUAGE NoImplicitPrelude #-}

module Timers where

import qualified Data.Map as M
import           Types


-- TODO(sandy): think about whether this should overwrite a timer
createTimer :: TimerType -> Time -> Game () -> Game ()
createTimer tt t cb =
  setGlobals $ timers . at tt ?~ Timer t cb

cancelTimer :: TimerType -> Game ()
cancelTimer tt =
  setGlobals $ timers . at tt .~ Nothing

updateTimers :: Time -> Game ()
updateTimers dt = do
  ts  <- getGlobals $ view timers
  ts' <- forOf traverse ts $ \t ->
           if _tTime t - dt <= 0
             then _tCallback t $> Nothing
             else pure . Just
                       $ t & tTime -~ dt

  setGlobals $
    timers .~ M.fromList (catMaybes . fmap sequence $ M.toList ts')

