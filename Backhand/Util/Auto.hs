{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Backhand.Util.Auto where

import Control.Auto
import Control.Auto.Blip
import Control.Auto.Blip.Internal


-- | Sample an auto's output when a blip stream outputs something.
--
-- Takes two autos, one which outputs a some value, and another which outputs a
-- blip stream. Produces an auto which outputs a blip with the contents of the
-- first auto whenever the blip stream outputs.
sample :: forall m a b c. (Monad m) =>
            Auto m a c
         -> Auto m a (Blip b)
         -> Auto m a (Blip (b, c))
sample valueAuto blipAuto = onJusts <<< arr extractMaybe <<< merged
  where
    merged :: Auto m a (Maybe b, c)
    merged = (asMaybes <<< blipAuto) &&& valueAuto
    extractMaybe :: (Maybe b, c) -> Maybe (b, c)
    extractMaybe (Just b, c) = Just (b, c)
    extractMaybe _ = Nothing


-- | Takes a @(f a, b)@ tuple, where @f@ is a functor, and wraps the whole tuple
-- in the functor.
fstF :: (Functor f) => (f a, b) -> f (a, b)
fstF (fa, b) = fmap (, b) fa

-- | Like `fstF`, but flipped.
sndF :: (Functor f) => (a, f b) -> f (a, b)
sndF (a, fb) = fmap (a, ) fb

-- | Takes a `Blip (Blip a, Blip b)` tuple and joins the outer blip with the
-- inner blips.
joinBothB :: Blip (Blip a, Blip b) -> (Blip a, Blip b)
joinBothB (Blip (a, b)) = (a, b)
joinBothB NoBlip = (NoBlip, NoBlip)

-- | A version of `joinB` to be used with tuples output by the `emitEithers`
-- function.
joinBT :: Monad m => Auto m (Blip (Blip a, Blip b)) (Blip a, Blip b)
joinBT = arr match
  where
    match (Blip t) = t
    match NoBlip = (NoBlip, NoBlip)

-- | Delayed version of `scanB`.
-- scanBD_ :: Monad m => Auto m (Blip a)
scanBD_ :: Monad m => (b -> a -> b) -> b -> Auto m (Blip a) b
scanBD_ f i = lastVal_ i <<< scanB_ f i
-- scanBD_ f = mkState_ func
--   where
--     func (Blip x) s = (s, f s x)
--     func NoBlip s = (s, s)
