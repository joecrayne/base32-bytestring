-- |
--   Copyright   :  (c) Joe Crayne 2020
--   License     :  BSD3
--   Maintainer  :  oh.hello.joe@gmail.com
--   Stability   :  stable
--   Portability :  portable
--
-- This module implements a variation [1] of Base32 created by Douglas
-- Crockford.  It excludes the letters I, L, and O to avoid confusion with
-- digits. It also excludes the letter U to reduce the likelihood of accidental
-- obscenity.
--
-- [1]: https://www.crockford.com/base32.html
--
{-# LANGUAGE BangPatterns #-}
module Data.ByteString.Base32.Crockford
       ( Base32c
       , encode
       , decode
       , decodeLenient
       ) where

import Data.ByteString as BS
import Data.ByteString.Base32.Internal
import Data.List as L
import Data.ByteString.Char8 as BC

-- | Crockford-Base32 encoded bytestring.
type Base32c = ByteString

encTable :: EncTable
encTable = BC.pack "0123456789abcdefghjkmnpqrstvwxyz"

decW5 :: Word8 -> Word5
decW5 !x = case allowSubst (toLower x) `BS.elemIndex` encTable of
  Just y  -> fromIntegral y
  Nothing -> invIx

allowSubst :: Word8 -> Word8
allowSubst 0x6F = 0x30 -- 'o' -> '0'
allowSubst 0x69 = 0x31 -- 'i' -> '1'
allowSubst 0x6C = 0x31 -- 'l' -> '1'
allowSubst 0x75 = 0x76 -- 'u' -> 'v'
allowSubst x    = x

toLower :: Word8 -> Word8
toLower x
    | 0x41 <= x && x <= 0x5A  = x + 0x20
    | otherwise               = x

-- | Encode an arbitrary bytestring into Crockford-base32 form.
encode :: ByteString -> Base32c
encode = unpack5 False encTable

decTable :: DecTable
decTable = BS.pack $ L.map decW5 [0 .. 255]

-- | Decode a z-base32 encoded bytestring. This function is
-- case-insensitive.
decode :: Base32c -> Either String ByteString
decode = pack5 decTable

-- | The same as 'decode' but with additional leniency: decodeLenient
-- will skip non-alphabet characters.
decodeLenient :: Base32c -> Either String ByteString
decodeLenient = pack5Lenient decTable
