module DropPriv (status, PrivStatus(..), dropUidGid) where

data PrivStatus = PrivStatus deriving Eq

instance Show PrivStatus where
    show PrivStatus = "NOOP for Windows. Privileges not dropped!"

status :: IO PrivStatus
status = return PrivStatus

dropUidGid :: Either String a -> Either String b -> IO ()
dropUidGid uid' gid' = return ()
