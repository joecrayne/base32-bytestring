-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  stable
--   Portability :  portable
--
--   Efficient encoding and decoding of base32hex encoded bytestring
--   according to RFC 4648. <http://tools.ietf.org/html/rfc4648>
--
--   This module recommended to be imported as @import
--   Data.ByteString.Base32.Hex as Base32Hex@ to avoid name clashes
--   with @Data.ByteString.Base32@.
--
{-# LANGUAGE BangPatterns #-}
module Data.ByteString.Base32.Z
       ( Base32z
       , encode
       , decode
       , decodeLenient
       ) where

import Data.ByteString as BS
import Data.ByteString.Base32.Internal
import Data.List as L
import Data.ByteString.Char8 as BC

-- | z-Base32 encoded bytestring.
type Base32z = ByteString

encTable :: EncTable
encTable = BC.pack "ybndrfg8ejkmcpqxot1uwisza345h769"

decW5 :: Word8 -> Word5
decW5 !x = case x `BS.elemIndex` encTable of
  Just y -> fromIntegral y
  Nothing -> invIx

-- | Encode an arbitrary bytestring into z-base32 form.
encode :: ByteString -> Base32z
encode = unpack5 False encTable

decTable :: DecTable
decTable = BS.pack $ L.map decW5 [minBound .. maxBound]

-- | Decode a z-base32 encoded bytestring. This function is
-- case-insensitive.
decode :: Base32z -> Either String ByteString
decode = pack5 decTable

-- | The same as 'decode' but with additional leniency: decodeLenient
-- will skip non-alphabet characters.
decodeLenient :: Base32z -> Either String ByteString
decodeLenient = pack5Lenient decTable
