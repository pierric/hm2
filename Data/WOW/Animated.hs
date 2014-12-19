{-# LANGUAGE TypeFamilies, EmptyDataDecls, FlexibleInstances, FlexibleContexts, BangPatterns #-}
module Data.WOW.Animated(Animated, PackedFloat, PackedQuaternion,newAnimated,at) where

import Graphics.Rendering.OpenGL.GL.Tensor
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
                          -> [Maybe BS.ByteString]     -- AnimFile
                          -> Animated a
newAnimated = newAnimated' undefined

at :: KeyStore a => Animated a
                 -> Int                          -- index
                 -> Int                          -- time
                 -> KeyType a                    -- default value
                 -> KeyType a
at = at' undefined

newAnimated' :: KeyStore a => a -> AnimationBlock -> [Int] -> BS.ByteString -> [Maybe BS.ByteString] -> Animated a
newAnimated' unused (AnimationBlock typ seq_ ntime otime nkey okey) global mpq anim =
  let -- get (number of timestamps,offset) paris, from offset of timeblock
      !ht    = bunchOf ntime otime getHeader
      -- get (number of keys, offset) paris, from offset of keyblock
      !hk    = bunchOf nkey  okey  getHeader
      -- get data for each of (number,offset) pairs
      !times = map (\(i,(nentry,oentry)) -> bunchOf' i nentry oentry getUInt) $ zip [0..] ht
      -- get data for each of (number,offset) paris
      !keys  = map (\(i,(nentry,oentry)) -> bunchOf' i nentry oentry (getKey unused)) $ zip [0..] hk
      -- global sequence
      !gs    = if seq_ == -1 then Nothing else assert (0 <= seq_ && seq_ < length global) (Just $ global!!seq_)
  in  -- not every bone have data of every animation
      assert (ntime == nkey) $ 
      case () of 
        _ | typ == 0 || typ == 1 -> 
              AnimatedLinear  gs times keys
        _ | typ == 2             -> 
              AnimatedHermite gs times (every3 keys) (every3 (tail keys)) (every3 (drop 2 keys))
        _                        ->
              error "Unknown type of animation"

    where
      bunchOf  :: Int -> Int -> Get a -> [a]
      bunchOf  s o g = getBunchOf s g (BS.drop (fromIntegral o) mpq)
      bunchOf' :: Int -> Int -> Int -> Get a -> [a]
      bunchOf' i s o g = case anim !! i of
                           Nothing -> getBunchOf s g (BS.drop (fromIntegral o) mpq)
                           Just x  -> getBunchOf s g (BS.drop (fromIntegral o) x)
      getHeader :: Get (Int,Int)
      getHeader = return (,) `ap` getUInt `ap` getUInt
      every3 (x:_:_:r) = x:every3 r
      every3 _ = []

at' :: KeyStore a => a -> Animated a -> Int -> Int -> KeyType a -> KeyType a
at' unused ani idx time default_
    | idx >= length (a_data_ ani) || idx >= length (a_times_ ani)
        = default_
    | otherwise
        = -- assert (idx >=0 && idx < length (a_data_ ani) && idx < length (a_times_ ani)) $
          assert (idx >= 0) $
          if (length dt > 1)
          then let ps    = filter (\(s',e') -> fst s' <= tt && tt < fst e') $ zip dt (tail dt)
                   (s,e) = if null ps 
                           then (dt!!0,dt!!1)  -- not found in the sequence, take the first one
                           else head ps 
                   r     = fromIntegral (tt - fst s) / fromIntegral (fst e - fst s)
                   -- show' (a,b) = "("++show a++","++ showK unused b++")"
               in  {--trace (show' s ++show' e++"~"++show r) $ --} interpolate ani (snd s) (snd e) r
          else if not (null dt)
               then {--trace "first" $ --} snd $ head dt
               else {--trace "default" $ --} default_
    where 
      mt = last $ a_times_ ani !! idx
      tt = case a_global_ ani of 
             Nothing -> time `mod` mt
             Just 0  -> 0
             Just mg -> time `mod` (min mt mg)
      dt = zip (a_times_ ani !! idx) (a_data_ ani !! idx)
      interpolate (AnimatedNone _ _ _) s _ _ = s
      interpolate (AnimatedLinear _ _ _) s e r = lerp unused s e r
      interpolate (AnimatedHermite _ _ _ _ _) _ _ _ = assert False undefined

class KeyStore a where
    type KeyType a :: *
    getKey :: a -> Get (KeyType a)
    lerp   :: a -> KeyType a -> KeyType a -> Float -> KeyType a
    showK  :: a -> KeyType a -> String

instance KeyStore PackedFloat where
    type KeyType PackedFloat = Float
    getKey _ = do t <- getUShort :: Get Int
                  return (fromIntegral t / 32767.0)
    lerp   _ = V.lerp
    showK  _ = show

instance KeyStore Float where
    type KeyType Float = Float
    getKey _ = getFloat
    lerp   _ = V.lerp
    showK  _ = show
    
instance KeyStore (Vector3 Float) where
    type KeyType (Vector3 Float) = (Vector3 Float)
    getKey _ = get
    lerp   _ = V.lerp
    showK  _ = show

instance KeyStore PackedQuaternion where
    type KeyType PackedQuaternion = Quaternion
    getKey _ = do x <- getUShort :: Get Int
                  y <- getUShort :: Get Int
                  z <- getUShort :: Get Int
                  w <- getUShort :: Get Int
                  let conv a = fromIntegral (if a<0 then a+32768 else a-32767) / 32767.0
                  return $ Quaternion (conv x, conv y, conv z,conv w)
    lerp   _ = V.lerp
    showK  _ = show

instance KeyStore Quaternion where
    type KeyType Quaternion = Quaternion
    getKey _ = do x <- getFloat
                  y <- getFloat
                  z <- getFloat
                  w <- getFloat
                  return $ Quaternion (x,y,z,w)
    lerp   _ = slerp
    showK  _ = show
