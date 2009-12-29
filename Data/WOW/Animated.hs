{-# LANGUAGE TypeFamilies, EmptyDataDecls, FlexibleInstances #-}
module Data.WOW.Animated where

import Data.Tensor
import Data.Binary(get,Get)
import Control.Exception
import qualified Data.ByteString.Lazy as BS
import Control.Monad(ap)
import qualified Data.VectorSpace as V

import Data.WOW.Utils
import Data.WOW.Quaternion
import Data.WOW.ModelDef

data Animated a = AnimatedNone   { a_global_ :: Maybe Int
                                 , a_times_  :: [[Int]]
                                 , a_data_   :: [[KeyType a]]
                                 }
                | AnimatedLinear { a_global_ :: Maybe Int
                                 , a_times_  :: [[Int]]
                                 , a_data_   :: [[KeyType a]]
                                 }
                | AnimatedHermite{ a_global_ :: Maybe Int
                                 , a_times_  :: [[Int]]
                                 , a_data_
                                 , a_in_
                                 , a_out_    :: [[KeyType a]]
                                 }

data PackedFloat
data PackedQuaternion

newAnimated :: KeyStore a => AnimationBlock
                          -> [Int]               -- global sequence
                          -> BS.ByteString       -- MPQ 
                          -> [BS.ByteString]     -- AnimFile
                          -> Animated a
newAnimated = newAnimated' undefined

at :: KeyStore a => Animated a
                 -> Int                          -- index
                 -> Int                          -- time
                 -> KeyType a
at = at' undefined

newAnimated' :: KeyStore a => a -> AnimationBlock -> [Int] -> BS.ByteString -> [BS.ByteString] -> Animated a
newAnimated' unused block@(AnimationBlock typ seq ntime otime nkey okey) global mpq anim =
  let -- get (number of timestamps,offset) paris, from offset of timeblock
      ht    = bunchOf ntime otime getHeader
      -- get data for each of (number,offset) pairs
      times = map (\(i,(nentry,oentry)) -> bunchOf' i nentry oentry getUShort) $ zip [0..] ht
      -- get (number of keys, offset) paris, from offset of keyblock
      hk    = bunchOf nkey  okey  getHeader
      -- get data for each of (number,offset) paris
      keys  = map (\(i,(nentry,oentry)) -> bunchOf' i nentry oentry (getKey unused)) $ zip [0..] hk
      -- global sequence
      gs    = if seq == -1 then Nothing else assert (0 <= seq && seq < length global) (Just $ global!!seq)
  in  case () of 
        _ | typ == 0 || typ == 1 -> 
              AnimatedLinear  gs times keys
        _ | typ == 2             -> 
              AnimatedHermite gs times (every3 keys) (every3 (tail keys)) (every3 (drop 2 keys))

    where
      bunchOf  s o g = getBunchOf s g (BS.drop (fromIntegral o) mpq)
      bunchOf' i s o g = case anim of
                           [] -> bunchOf s o g
                           _  -> assert (i < length anim) $
                                 getBunchOf s g (BS.drop (fromIntegral o) $ anim !! i)
      getHeader :: Get (Int,Int)
      getHeader = return (,) `ap` getUShort `ap` getUShort
      every3 (x:y:z:r) = x:every3 r
      every3 _ = []

at' :: KeyStore a => a -> Animated a -> Int -> Int -> KeyType a
at' unused ani idx time =
    assert (idx >=0 && idx < length (a_data_ ani) && idx < length (a_times_ ani)) $
    if (length dt > 1)
    then let ps    = filter (\(s,e) -> fst s <= tt && tt < fst e) $ zip dt (tail dt)
             (s,e) = assert (length ps > 0) (head ps)
             r     = fromIntegral (time - fst s) / fromIntegral (fst e - fst s)
         in  interpolate ani (snd s) (snd e) r
    else assert (not (null dt)) (snd $ head dt)
    
    where 
      tt = case a_global_ ani of 
             Nothing -> time
             Just 0  -> 0
             Just mt -> time `mod` mt
      dt = zip (a_times_ ani !! idx) (a_data_ ani !! idx)
      interpolate (AnimatedNone _ _ _) s e r = s
      interpolate (AnimatedLinear _ _ _) s e r = lerp unused s e r
      interpolate (AnimatedHermite _ _ _ _ _) s e r = assert False undefined

class KeyStore a where
    type KeyType a :: *
    getKey :: a -> Get (KeyType a)
    lerp   :: a -> KeyType a -> KeyType a -> Float -> KeyType a

instance KeyStore PackedFloat where
    type KeyType PackedFloat = Float
    getKey _ = do t <- getUShort
                  return (fromIntegral t / 32767.0)
    lerp   _ = V.lerp

instance KeyStore Float where
    type KeyType Float = Float
    getKey _ = getFloat
    lerp   _ = V.lerp
    
instance KeyStore (Vector3 Float) where
    type KeyType (Vector3 Float) = (Vector3 Float)
    getKey _ = get
    lerp   _ = V.lerp

instance KeyStore PackedQuaternion where
    type KeyType PackedQuaternion = Quaternion
    getKey _ = do x <- getUShort
                  y <- getUShort
                  z <- getUShort
                  w <- getUShort
                  let conv a = fromIntegral (if x<0 then x+32768 else x-32767) / 32767.0
                  return $ Quaternion (conv x, conv y, conv z,conv w)
    lerp   _ = V.lerp

instance KeyStore Quaternion where
    type KeyType Quaternion = Quaternion
    getKey _ = do x <- getFloat
                  y <- getFloat
                  z <- getFloat
                  w <- getFloat
                  return $ Quaternion (x,y,z,w)
    lerp   _ = slerp