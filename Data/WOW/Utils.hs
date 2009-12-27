{-# OPTIONS -XFlexibleInstances -XTypeFamilies #-}
module Data.WOW.Utils where

import Data.Binary
import Data.Binary.IEEE754
import Data.Binary.Get
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Internal(w2c)
import Data.Int
import Data.Tensor
import Control.Monad(ap)
import Data.VectorSpace

import Data.WOW.Quaternion

getCChar  :: Get Char
getCChar  = get
getUChar  :: Integral i => Get i
getUChar  = fmap fromIntegral getWord8
getUShort :: Integral i => Get i
getUShort = fmap (fromIntegral . (fromIntegral :: Word16 -> Int16)) getWord16le
getUInt   :: Integral i => Get i
getUInt   = fmap fromIntegral getWord32le
getFloat  :: Get Float
getFloat  = getFloat32le

getString :: Get String
getString = fmap BS.unpack getLazyByteStringNul

getBunchOf :: Int -> Get a -> BS.ByteString -> [a]
getBunchOf cnt g content = runGet (sequence $ replicate cnt g) content


instance Binary (Vector3 Float) where
    get   = return Vector3 `ap` getFloat32le `ap` getFloat32le `ap` getFloat32le
    put _ = error "cannot save Vector3"

instance Binary (Vector2 Float) where
    get   = return Vector2 `ap` getFloat32le `ap` getFloat32le
    put _ = error "cannot save Vector2"


triple_f (a,b,c) = Vector3 a b c
triple_t (Vector3 a b c) = (a,b,c)

instance AdditiveGroup (Vector3 Float) where
    zeroV   = triple_f zeroV
    a ^+^ b = triple_f $ triple_t a ^+^ triple_t b
    negateV = triple_f . negateV . triple_t

instance VectorSpace (Vector3 Float) where
    type Scalar (Vector3 Float) = Float
    s *^ (Vector3 u v w) = Vector3 (s*^u) (s*^v) (s*^w)
    