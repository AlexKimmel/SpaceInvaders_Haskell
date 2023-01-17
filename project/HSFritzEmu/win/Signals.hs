module Signals(installAbortSignal) where

import Foreign.C.Types
import Foreign

sigInt = 2
sigTerm = 15

foreign import ccall "wrapper"
  wrapper :: (CInt -> IO ()) -> IO (FunPtr (CInt -> IO ()))

foreign import ccall safe "signal.h signal"
  signal :: CInt -> FunPtr (CInt -> IO ()) -> IO CInt

installAbortSignal :: IO () -> IO ()
installAbortSignal io = do
  fptr <- wrapper $ \_ -> io
  signal sigInt fptr
  signal sigTerm fptr
  return ()
