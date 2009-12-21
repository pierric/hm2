module GL.Texture where

import Data.Bits
import qualified Data.ByteString as BS
import Data.ByteString.Internal
import Data.Word
import Data.Array(Array,(!))
import Control.Monad(forM_)
import Foreign.Ptr(Ptr,plusPtr)
import Foreign.Storable(poke)
import Foreign.Marshal.Array(allocaArray)
import Foreign.ForeignPtr
import qualified Graphics.Rendering.OpenGL.GL as GL

import BLP
import GL.Types

newTexture :: TextureType -> BLP -> IO Texture
newTexture typ raw = do
  [texture] <- GL.genObjectNames 1
  GL.textureBinding (glTextureType typ) GL.$= Just texture
  case raw of
    BLPp _ pal size@(w,h) ab raw -> allocaArray (w*h) (setupLayerP pal size ab raw)
    BLPc _ cty size@(w,h) raw    -> setupLayerC cty size raw
  GL.textureFilter (glTextureType typ) GL.$= ((GL.Linear',Nothing),GL.Linear')
  return (Texture {--(blp_name raw)--} typ texture)

    where fix :: Word32 -> Word32 -> Word32 
          fix v a = shiftR (v .&. 0x00FF0000) 16 .|. (v .&. 0x0000FF00) .|. shiftL (v .&. 0x000000FF) 16 .|. shiftL a 24

          alpha :: Int -> BS.ByteString -> [Word32]
          alpha 0 raw = repeat 0xff
          alpha 1 raw = concatMap (\r -> map (\i -> if testBit r i then 0xff else 0) [0..7]) $ BS.unpack raw
          alpha 8 raw = map fromIntegral (BS.unpack raw)

          halve :: (Int,Int) -> [(Int,Int)]
          halve (1,1) = repeat (1,1)
          halve (1,n) = (1,n) : halve (1, div n 2)
          halve (m,1) = (m,1) : halve (div m 2, 1)
          halve (m,n) = (m,n) : halve (div m 2, div n 2)

          setupLayerP :: Array Int Word32 -> (Int,Int) -> Int -> [BS.ByteString] -> Ptr Word32 -> IO ()
          setupLayerP pal (w,h) ab raw buf =
            forM_ (zip3 [1..] (halve (w,h)) raw) (\(l,(w',h'),r) -> do
              let s = w'*h'
              forM_ (zip3 [1..s] (BS.unpack r) (alpha ab (BS.drop s r))) (\(o,i,a) ->
                poke (buf `plusPtr` o) (fix (pal ! fromIntegral i) a))
              GL.texImage2D Nothing GL.NoProxy
                            l GL.RGBA8 (GL.TextureSize2D (fromIntegral w') (fromIntegral h')) 0 
                            (GL.PixelData GL.RGBA GL.UnsignedByte buf) )

          setupLayerC :: CTYPE -> (Int,Int) -> [BS.ByteString] -> IO ()
          setupLayerC ctype (w,h) raw = 
            forM_ (zip3 [1..] (halve (w,h)) raw) (\(l,(w',h'),r) -> do
              let (buf,lb,hb) = toForeignPtr r
              withForeignPtr buf (\pb ->
                GL.compressedTexImage2D Nothing GL.NoProxy
                                        l (GL.TextureSize2D (fromIntegral w') (fromIntegral h')) 0
                                        (GL.CompressedPixelData (glCompressedTextureFormat ctype) 
                                                                (fromIntegral $ hb - lb) 
                                                                pb)))
