module Network.MateLight.ListFrame where
import Network.MateLight
import Data.Word
import qualified Data.ByteString.Lazy.Builder as BSL
import qualified Data.ByteString.Short as BSS

data Pixel = Pixel {
   pixR :: Word8
  ,pixG :: Word8
  ,pixB :: Word8
} deriving (Eq, Ord, Show, Read)
renderPixel :: Pixel -> [Word8]
renderPixel (Pixel a b c) = [a, b, c]

newtype ListFrame = ListFrame [[Pixel]] deriving (Eq, Ord, Show, Read)
instance Frame ListFrame where
   theData (ListFrame pixels) = BSL.toLazyByteString $ foldMap (foldMap (BSL.shortByteString . BSS.pack . renderPixel)) pixels
   dimension (ListFrame pixels) = (maximum $ map length pixels, length pixels)
