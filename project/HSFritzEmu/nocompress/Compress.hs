module Compress (compress) where
import Data.Conduit
import Data.ByteString
import Control.Monad.Trans.Resource
import qualified Data.Conduit.List as C

compress :: MonadResource m => Conduit ByteString m ByteString
compress = C.map id
