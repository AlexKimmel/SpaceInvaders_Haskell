module Compress (compress) where
import Data.Conduit
import Data.ByteString
import Control.Monad.Trans.Resource
import qualified Data.Conduit.LZ4 as L

compress :: MonadResource m => Conduit ByteString m ByteString
compress = L.compress Nothing
