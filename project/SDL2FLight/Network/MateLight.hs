{-# LANGUAGE GeneralizedNewtypeDeriving,RankNTypes,ScopedTypeVariables,ExistentialQuantification #-}
module Network.MateLight (
   EventProviderT
  ,parseAddress
  ,Config(..)
  ,runMateM
  ,Frame(..)
  ,EventT(..)
  ,MateMonad()
  ,mHPutStrLn
  ,mPutStrLn
  ) where
import Data.Word
import System.IO
import Control.Monad.State
import Control.Monad.Reader
import Data.IP
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString.Lazy as NBSL
import qualified Data.ByteString.Lazy as BSL
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Data.Typeable
import qualified Network.MateLight.Debug as Debug
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE logLock #-}
logLock :: MVar ()
logLock = unsafePerformIO $ newMVar ()

mHPutStrLn :: Handle -> String -> IO ()
mHPutStrLn handle str = withMVar logLock (\() -> hPutStrLn handle str)
mPutStrLn = mHPutStrLn stdout

{-
 - TODO: Exception handling?
 - -- change the whole dimension thing
 -}

class Frame f where
  theData :: f -> BSL.ByteString
  dimension :: f -> (Int, Int)

data EventT = forall a. (Typeable a, Show a) => EventT String a
instance Show EventT where
  show (EventT mod a) = "Event " ++ show mod ++ " " ++ show a

type EventProviderT = TChan EventT -> IO ()

data Config = Config {
   cAddr :: IP
  ,cPort :: Sock.PortNumber
  ,cDimension :: (Int, Int)
  ,cStepTime :: Maybe Int
  ,cSynchronized :: Bool -- aggregate events, send only at step
  ,cEventProviders :: [EventProviderT]
}

parseAddress :: String -> Maybe IP
parseAddress str = maybe (IPv4 `fmap` (readMaybe str :: Maybe IPv4)) (fmap IPv6) (readMaybe str)
  where readMaybe str = case reads str of { [(a, "")] -> Just a; _ -> Nothing }

newtype MateMonad f s m a = MateMonad {
  unMateMonad :: (StateT s (ReaderT f m) a)
  } deriving (Applicative, Functor, Monad, MonadIO, MonadState s, MonadReader f)

whileM :: Monad m => m Bool -> m a -> m [a]
whileM cond action = do
  c <- cond
  if c then do
    res <- action
    ress <- whileM cond action
    return $ res : ress
   else
    return []

runMateM :: forall f s . (Frame f) => Config -> ([EventT] -> MateMonad f s IO f) -> s -> IO ()
runMateM conf fkt s = do
  -- Change socket code
  sock <- Sock.socket Sock.AF_INET Sock.Datagram Sock.defaultProtocol 
  case cAddr conf of
    IPv4 ip -> Sock.connect sock $ Sock.SockAddrInet (cPort conf) (toHostAddress ip)
    IPv6 ip -> Sock.connect sock $ Sock.SockAddrInet6 (cPort conf) 0 (toHostAddress6 ip) 0
  chanStepper <- newChan :: IO (Chan ())
  case cStepTime conf of
    Nothing -> return ()
    Just time -> dupChan chanStepper >>= forkIO . stepper time >> return ()
  chanEvent <- newTChanIO :: IO (TChan EventT)
  threads <- forM (cEventProviders conf) $ \ep -> do
    c <- atomically (dupTChan chanEvent)
    handle <- newEmptyMVar
    forkFinally (ep c) $ \_ -> putMVar handle ()
    return handle
  forkIO $ (if cSynchronized conf then acumulatingCaller else caller) sock chanStepper chanEvent undefined s
  when (null threads) $ do -- this whole do block stinks, but then so does capturing keyboard events
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    atomically (dupTChan chanEvent) >>= \ch -> forever $ do
      c <- getChar
      atomically $ writeTChan ch $ EventT "KEYBOARD" [c]
  mapM_ takeMVar threads -- wait for event provider threads
  Sock.close sock -- use bracket
  where
  stepper :: Int -> Chan () -> IO ()
  stepper delay chan = do
    writeChan chan ()
    threadDelay delay
    stepper delay chan
  acumulatingCaller :: Sock.Socket -> Chan () -> TChan EventT -> f -> s -> IO ()
  acumulatingCaller sock chanStepper chanEvent oldFrame curState = do
    () <- readChan chanStepper
    events <- atomically $ whileM (not `fmap` isEmptyTChan chanEvent) (readTChan chanEvent)
    Debug.debug $ "Will call with events: " ++ show events
    (newFrame, newState) <- runReaderT (runStateT (unMateMonad (fkt events)) curState) oldFrame :: IO (f, s)
    sendFrame sock newFrame
    acumulatingCaller sock chanStepper chanEvent newFrame newState
  caller :: Sock.Socket -> Chan () -> TChan EventT -> f -> s -> IO ()
  caller sock chanStepper chanEvent oldFrame oldState = do
    unitedChan <- newChan :: IO (Chan (Either () EventT))
    dupChan unitedChan >>= \dupped -> forkIO $ forever $ do
      () <- readChan chanStepper
      writeChan dupped $ Left ()
    dupChan unitedChan >>= \dupped -> forkIO $ forever $ do
      event <- atomically $ readTChan chanEvent
      writeChan dupped $ Right event
    let helper oldFrame curState = do
          msg <- readChan unitedChan
          let realEvents = [ev | Right ev <- [msg]]
          Debug.debug $ "Will call with events: " ++ show realEvents
          (newFrame, newState) <- runReaderT (runStateT (unMateMonad (fkt realEvents)) curState) oldFrame :: IO (f, s)
          sendFrame sock newFrame
          helper newFrame newState
    helper oldFrame oldState
  sendFrame :: Frame f => Sock.Socket -> f -> IO ()
  sendFrame sock frame = if BSL.length bs == fromIntegral (x * y * 3) then NBSL.sendAll sock $ theData frame else hPutStrLn stderr "function returned incorrect frame"
    where
    (x, y) = cDimension conf
    bs = theData frame
