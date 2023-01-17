{-# LANGUAGE ScopedTypeVariables #-}
module Types(Frame(..), Message(..),isInetBS,isInet6BS) where

import qualified Network.WebSockets as WSock
import qualified Network.Socket as Sock
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Word
import Data.Bits
import Test.QuickCheck

data Frame = Frame {
   source :: Sock.SockAddr
  ,message :: BS.ByteString
} deriving (Show, Eq)

instance Arbitrary Sock.SockAddr where
  arbitrary = oneof [inet, inet6]
    where
    inet = arbitrary >>= \(pn :: Word16) -> arbitrary >>= \ha -> return $ Sock.SockAddrInet (fromIntegral pn) ha
    inet6 = arbitrary >>= \(pn :: Word16) -> arbitrary >>= \fi -> arbitrary >>= \ha -> arbitrary >>= \si -> return $ Sock.SockAddrInet6 (fromIntegral pn) fi ha si
instance Arbitrary Message where -- tests only MCUpdate
  arbitrary = MCUpdate `fmap` arbitrary
prop_sockLength sa@(Sock.SockAddrInet _ _) = BSL.length (WSock.toLazyByteString sa) == 7
prop_sockLength sa@(Sock.SockAddrInet6 _ _ _ _) = BSL.length (WSock.toLazyByteString sa) == 27
prop_sock sa = WSock.fromLazyByteString (WSock.toLazyByteString sa) == (sa :: Sock.SockAddr)
prop_sockAddrLst msg = WSock.fromLazyByteString (WSock.toLazyByteString msg) == (msg :: Message)

data Message = MFrame Frame | MCUpdate [Sock.SockAddr] deriving (Show, Eq)
instance WSock.WebSocketsData Message where
  fromLazyByteString bs | BSL.length bs > 0 && BSL.head bs == 0 = MFrame $ Frame (error "no sockAddr") $ BSL.toStrict $ BSL.tail bs
  fromLazyByteString bs | BSL.length bs > 0 && BSL.head bs == 1 = MCUpdate $ map WSock.fromLazyByteString $ split tl
    where
    tl = BSL.tail bs
    split bs | BSL.null bs = []
            | BSL.length bs >= 7 && BSL.head bs == 0 = BSL.take 7 bs : split (BSL.drop 7 bs)
            | BSL.length bs >= 27 && BSL.head bs == 1 = BSL.take 27 bs : split (BSL.drop 27 bs)
  toLazyByteString (MFrame frame) = 0 `BSL.cons` BSL.fromStrict (message frame)
  toLazyByteString (MCUpdate clients) = 1 `BSL.cons` BSL.concat (map WSock.toLazyByteString clients)

isInetBS :: BSL.ByteString -> Bool
isInetBS bs = BSL.length bs == 7 && BSL.head bs == 0
isInet6BS :: BSL.ByteString -> Bool
isInet6BS bs = BSL.length bs == 27 && BSL.head bs == 1

instance WSock.WebSocketsData Sock.SockAddr where
  fromLazyByteString bs | isInetBS bs = Sock.SockAddrInet pn ha
    where
    [pn1, pn0, ha3, ha2, ha1, ha0] = BSL.unpack $ BSL.tail bs
    pn = fromIntegral (bytesToWord $ [pn1, pn0] :: Word16)
    ha = bytesToWord [ha3, ha2, ha1, ha0]
  fromLazyByteString bs | isInet6BS bs = Sock.SockAddrInet6 pn fi ha si
    where
    rs = BSL.unpack $ BSL.tail bs
    pn = fromIntegral $ (bytesToWord $ take 2 rs :: Word16)
    fi = bytesToWord $ take 4 $ drop 2 rs
    [ha15, ha14, ha13, ha12, ha11, ha10, ha9, ha8, ha7, ha6, ha5, ha4, ha3, ha2, ha1, ha0] = take 16 $ drop 6 rs
    ha = (bytesToWord [ha15, ha14, ha13, ha12], bytesToWord [ha11, ha10, ha9, ha8], bytesToWord [ha7, ha6, ha5, ha4], bytesToWord [ha3, ha2, ha1, ha0])
    si = bytesToWord $ drop 22 rs
  toLazyByteString (Sock.SockAddrInet pn ha) = 0 `BSL.cons` BSL.append bsPn bsHa
    where
    bsPn = BSL.pack $ wordToBytes (fromIntegral pn :: Word16)
    bsHa = BSL.pack $ wordToBytes ha
  toLazyByteString (Sock.SockAddrInet6 pn fi (ha3, ha2, ha1, ha0) si) = 1 `BSL.cons` BSL.concat [bsPn, bsFi, bsHa, bsSi]
    where
    bsPn = BSL.pack $ wordToBytes (fromIntegral pn :: Word16)
    bsFi = BSL.pack $ wordToBytes fi
    bsHa = BSL.pack $ concatMap wordToBytes [ha3, ha2, ha1, ha0]
    bsSi = BSL.pack $ wordToBytes si

-- to not use BS builder because of two way requirement
wordToBytes :: (FiniteBits a, Integral a) => a -> [Word8]
wordToBytes w = map (\i -> fromIntegral $ w `shiftR` (i * 8)) [byteSize -1 , byteSize - 2 .. 0]
  where byteSize = finiteBitSize w `div` 8
bytesToWord :: forall a. (FiniteBits a, Integral a) => [Word8] -> a
bytesToWord = fst . foldr (\b (r, c) -> (r .|. fromIntegral b `shiftL` c :: a, c + 8)) (0 :: a, 0 :: Int)
