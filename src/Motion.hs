{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Motion where

import           Bezier
import           Control.Monad.Coroutine
import           Control.Monad.Coroutine.SuspensionFunctors hiding (Reader)
import           Control.Monad.Trans                        (lift)
import           Control.Monad.Trans.Reader
import           Control.Monad.Writer                       (runWriter, tell)
import           Types


pumpMotion :: Time -> SomeRoom -> Actor s -> Game (Actor s)
pumpMotion dt room a =
  case view motion' a of
    Nothing -> pure a
    Just m  -> do
      let (pos, m', as) = runMotion dt m (view actorPos' a) room
      tell as
      pure $ a & actorPos' .~ pos
               & motion' .~ m'

emit :: GameAction -> Machine ()
emit a = Coroutine
       . ReaderT
       . const $ tell [a] >> pure (Right ())

wait :: Float -> Pos -> Machine ()
wait duration pos =
  when (duration >= 0) $ do
    dt <- request pos
    wait (duration - dt) pos

goto :: Float -> Pos -> Machine ()
goto duration = runBezier duration . pure

gotoVel :: Float -> Pos -> Machine ()
gotoVel duration = velBezier duration . pure

runBezier :: Float -> [Pos] -> Machine ()
runBezier duration v2s = do
    pos <- lift $ asks fst
    let b = bezier $ fmap fromIntegral <$> pos : v2s
    loop b 0
  where
    loop b t = do
      dt <- request . fmap round $ b (t / duration)
      when (dt + t < duration) . loop b $ dt + t

velBezier :: Float -> [Pos] -> Machine ()
velBezier velocity v2s = do
  pos <- lift $ asks fst
  runBezier (bezierLength (fmap fromIntegral <$> pos : v2s) / velocity) v2s

runMotion :: Float -> Motion -> Pos -> SomeRoom -> (Pos, Maybe Motion, [GameAction])
runMotion dt (Motion m) pos room =
  case runWriter . flip runReaderT (pos, room) . resume $ m dt of
    (Left (Request v2 c), as) -> (v2, Just $ Motion c, as)
    (Right (), as)            -> (pos, Nothing, as)

navigateTo :: Float -> Pos -> Machine ()
navigateTo vel dst = do
  (src, room) <- lift ask
  case navigate (view navmesh room) src dst of
    Just path -> mapM_ (gotoVel vel) path
    Nothing   -> pure ()
