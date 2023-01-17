{-# LANGUAGE ScopedTypeVariables,DeriveDataTypeable #-}
module DropPriv (status, DropPrivException(), PrivStatus(..), dropUidGid) where
import Control.Exception
import Data.Typeable
import Control.Applicative
import Control.Monad
import System.IO
import qualified System.Posix.User as Posix
import qualified System.Posix.Directory as Posix
import qualified System.Posix.Types as Posix

data DropPrivException = DropPrivException deriving (Typeable, Show)
instance Exception DropPrivException

data PrivStatus = PrivStatus {
   realUID :: Posix.UserID
  ,effectiveUID :: Posix.UserID
  ,realGID :: Posix.GroupID
  ,effectiveGID :: Posix.GroupID
  ,pwd :: FilePath
  } deriving (Show, Eq)

status :: IO PrivStatus
status = do
  uid <- Posix.getRealUserID
  euid <- Posix.getEffectiveUserID
  gid <- Posix.getRealGroupID
  egid <- Posix.getEffectiveGroupID
  pwd <- Posix.getWorkingDirectory
  return $ PrivStatus uid euid gid egid pwd

dropUidGid :: Either String Posix.UserID -> Either String Posix.GroupID -> IO ()
dropUidGid uid' gid' = (do
  uid <- either (fmap Posix.userID . Posix.getUserEntryForName) return uid'
  gid <- either (fmap Posix.groupID . Posix.getGroupEntryForName) return gid'
  {-dir <- Posix.homeDirectory <$> Posix.getUserEntryForID uid-}
  let dir = "/"
      wish = PrivStatus uid uid gid gid dir
  Posix.changeWorkingDirectory dir
  Posix.setGroupID gid
  Posix.setUserID uid
  cur <- status
  when (wish /= cur) $ throwIO DropPrivException) `catch` (\(e :: SomeException) -> do
    hPutStrLn stderr $ "Exception: " ++ show e
    throwIO e)
