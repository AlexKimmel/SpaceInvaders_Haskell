module Network.MateLight.Simple (
   Event(..)
  ,stringEvent
  ,runMate
  ,runMateRandom
  ,parseAddress
  ,Config(..)
  ,Frame(..)
  ,ListFrame(..)
  ,Pixel(..)
  ) where

import Network.MateLight
import Network.MateLight.ListFrame
import Data.Typeable
import Control.Monad.State
import System.Random

data Event a = Event String a deriving (Eq, Ord, Show, Read)

{-
 - The runMate function takes a config, a function, and an initial state s.
 - The function must take a list of events, a state, and produce a tuple of a frame f and next state s.
 - The purpose of the state is to communicate what should happen in the next frame.
 -}
runMate :: (Frame f) => Config -> ([Event String] -> s -> (f, s)) -> s -> IO ()
runMate conf fkt = runMateM conf $ state . fkt . map stringEvent

{-
 - Similar to the runMate function, but the function additionally must take an infinite list of random Ints as first argument.
 - These Ints can be used for example in games to generate a level.
 -}
runMateRandom :: (Frame f) => Config -> ([Int] -> [Event String] -> s -> (f, s)) -> s -> IO ()
runMateRandom conf fkt = runMateM conf $ \evs -> do
  ints <- liftIO $ newStdGen >>= return . randomRs (minBound, maxBound)
  state $ fkt ints (map stringEvent evs)

-- misc
castEvent :: Typeable a => EventT -> Maybe (Event a)
castEvent (EventT mod a) = Event mod `fmap` cast a
stringEvent :: EventT -> Event String
stringEvent (EventT mod a) = Event mod $ show a
