module Signals(installAbortSignal) where

import qualified System.Posix.Signals as Signals

installAbortSignal :: IO () -> IO ()
installAbortSignal io = do
  _ <- Signals.installHandler Signals.sigINT (Signals.CatchOnce io) Nothing
  _ <- Signals.installHandler Signals.sigTERM (Signals.CatchOnce io) Nothing
  return ()
