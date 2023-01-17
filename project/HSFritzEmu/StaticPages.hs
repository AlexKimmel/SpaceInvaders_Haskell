{-# LANGUAGE OverloadedStrings #-}
module StaticPages (staticPages, lookup, lookupWithDefault) where
import Prelude hiding (lookup)
import qualified Network.HTTP.Types.Status as Status
import qualified Network.HTTP.Types.Header as Header
import qualified Network.Wai as Wai
import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified System.FilePath.Posix as FP
import Data.Maybe

{-import qualified Network.Wai.Handler.Warp as Warp-}

staticPages :: M.Map FilePath BS.ByteString -> Maybe Wai.Application -> ([T.Text] -> M.Map [T.Text] LBS.ByteString -> Maybe LBS.ByteString) -> Wai.Application
staticPages staticFiles failure' lookup req resp = do
  {-print $ Wai.pathInfo req-}
  let page = lookup (Wai.pathInfo req) compatible
  maybe (failure req resp) (resp . Wai.responseLBS Status.status200 [(Header.hContentType, "text/html")]) page -- html only...
  where
  compatible = toCompatible staticFiles
  defaultFailure req resp = resp $ Wai.responseLBS Status.status404 [(Header.hContentType, "test/plain")] "Not Found"
  failure = fromMaybe defaultFailure failure'

lookupWithDefault :: [T.Text] -> [T.Text] -> M.Map [T.Text] LBS.ByteString -> Maybe LBS.ByteString
lookupWithDefault defaults k m = listToMaybe $ catMaybes $ M.lookup k m : map (\d -> M.lookup (k ++ [d]) m) defaults

lookup :: [T.Text] -> M.Map [T.Text] LBS.ByteString -> Maybe LBS.ByteString
lookup = M.lookup

toCompatible :: M.Map FilePath BS.ByteString -> M.Map [T.Text] LBS.ByteString
toCompatible = M.foldrWithKey (\k v -> M.insert (keyCompat k) (LBS.fromStrict v)) M.empty
  where
  removeSlash "" = ""
  removeSlash xs | last xs == '/' = init xs
                 | otherwise = xs
  keyCompat fp = map T.pack $ filter (not . null) $ map removeSlash $ FP.splitPath fp

{-main :: IO ()-}
{-main = Warp.run 8080 $ staticPages staticFiles-}
