module Data.WOW.GL.Texture where

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
import qualified Graphics.Rendering.OpenGL.Raw as GLR
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import Codec.Image.STB as STB
import Codec.Image.DDS as DDS

import Data.WOW.BLP
import Data.WOW.GL.Types
import Data.WOW.FileSystem
import Data.WOW.World

newTexture :: ResourceId -> IO Texture
newTexture fpath = do 
  image <- loadImage $ localFilePath fpath
  case image of 
    Left err  -> putStrLn err >> undefined
    Right img -> do [texture] <- GL.genObjectNames 1
                    GL.textureBinding GL.Texture2D GL.$= Just texture
                    STB.withImage img (\ptr (w,h) chn -> do
                      GLU.build2DMipmaps GL.Texture2D GL.RGBA' (fromIntegral w) (fromIntegral h)
                                         (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
                      GL.textureFilter GL.Texture2D GL.$= ((GL.Linear',Nothing),GL.Linear')
                      return (Texture tex_type_2d texture) )


newTextureFromBLP :: (GL.BindableTextureTarget t, GL.ParameterizedTextureTarget t) => t -> BLP -> IO Texture
newTextureFromBLP typ raw = do
  [texture] <- GL.genObjectNames 1
  GL.textureBinding typ GL.$= Just texture
  case raw of
    BLPp pal size@(w,h) ab raw_ -> allocaArray (w*h) (setupLayerP pal size ab raw_)
    BLPc cty size raw_          -> setupLayerC cty size raw_
  GL.textureFilter typ GL.$= ((GL.Linear',Nothing),GL.Linear')
  GL.textureWrapMode GL.Texture2D GL.R GL.$= (GL.Repeated, GL.Clamp)
  GL.textureWrapMode GL.Texture2D GL.S GL.$= (GL.Repeated, GL.ClampToEdge)
  GL.textureWrapMode GL.Texture2D GL.T GL.$= (GL.Repeated, GL.ClampToEdge)  
  return (Texture typ texture)

    where fix :: Word32 -> Word32 -> Word32 
          fix v a = shiftR (v .&. 0x00FF0000) 16 .|. (v .&. 0x0000FF00) .|. shiftL (v .&. 0x000000FF) 16 .|. shiftL a 24

          alpha :: Int -> BS.ByteString -> [Word32]
          alpha 0 raw_ = repeat 0xff
          alpha 1 raw_ = concatMap (\r -> map (\i -> if testBit r i then 0xff else 0) [0..7]) $ BS.unpack raw_
          alpha 8 raw_ = map fromIntegral (BS.unpack raw_)
          alpha _ _    = error "Unknown alpha type"

          halve :: (Int,Int) -> [(Int,Int)]
          halve (1,1) = repeat (1,1)
          halve (1,n) = (1,n) : halve (1, div n 2)
          halve (m,1) = (m,1) : halve (div m 2, 1)
          halve (m,n) = (m,n) : halve (div m 2, div n 2)

          setupLayerP :: Array Int Word32 -> (Int,Int) -> Int -> [BS.ByteString] -> Ptr Word32 -> IO ()
          setupLayerP pal (w,h) ab raw_ buf =
            forM_ (zip3 [0..] (halve (w,h)) raw_) (\(l,(w',h'),r) -> do
              let s = w'*h'
              forM_ (zip3 [0..] (BS.unpack r) (alpha ab (BS.drop s r))) (\(o,i,a) ->
                poke (buf `plusPtr` o) (fix (pal ! fromIntegral i) a))
              GL.texImage2D tex_type_2d GL.NoProxy
                            l GL.RGBA8 (GL.TextureSize2D (fromIntegral w') (fromIntegral h')) 0 
                            (GL.PixelData GL.RGBA GL.UnsignedByte buf) )

          setupLayerC :: CTYPE -> (Int,Int) -> [BS.ByteString] -> IO ()
          setupLayerC ctype (w,h) raw_ = do
            support <- supportTextureCompression
            forM_ (zip3 [0..] (halve (w,h)) raw_) (\(l,(w',h'),r) ->
              if not support
              then let t' = case ctype of DXT1' -> DXT1
                                          DXT3' -> DXT3
                                          DXT5' -> DXT5
                   in  withDecompressed t' (BS.unpack r) w' h' (\pp -> 
                         GL.texImage2D tex_type_2d GL.NoProxy l GL.RGBA'
                                       (GL.TextureSize2D (fromIntegral w') (fromIntegral h')) 0 
                                       (GL.PixelData GL.RGBA GL.UnsignedByte pp))
              else let (buf,offset,len) = toForeignPtr r
                       glctype = (glCompressedTextureFormat ctype)
                       blksz   = case ctype of DXT1' -> 8
                                               _     -> 16
                       sz      = ((w'+3)`div`4) * ((h'+3)`div`4) * blksz
                       (nw,nh) = if len == 8 && sz == 16 && w'==2 && h'==8 then (2,4) else (w',h')
                   in  withForeignPtr buf (\pb -> do
                         GL.compressedTexImage2D tex_type_2d GL.NoProxy l 
                                                 (GL.TextureSize2D (fromIntegral nw) (fromIntegral nh)) 0
                                                 (GL.CompressedPixelData glctype (fromIntegral len) 
                                                 (pb `plusPtr` offset))
                         err <- GL.get GLU.errors
                         forM_ err (putStrLn . show)))

glCompressedTextureFormat :: CTYPE -> GL.CompressedTextureFormat
glCompressedTextureFormat DXT1' = GL.CompressedTextureFormat GLR.gl_COMPRESSED_RGBA_S3TC_DXT1 
glCompressedTextureFormat DXT3' = GL.CompressedTextureFormat GLR.gl_COMPRESSED_RGBA_S3TC_DXT3
glCompressedTextureFormat DXT5' = GL.CompressedTextureFormat GLR.gl_COMPRESSED_RGBA_S3TC_DXT5

supportTextureCompression :: IO Bool
supportTextureCompression = do
  list <- GL.get GL.compressedTextureFormats 
  return $ (glCompressedTextureFormat DXT1' `elem` list) && 
           (glCompressedTextureFormat DXT3' `elem` list) &&
           (glCompressedTextureFormat DXT5' `elem` list)
           
{--
saveTexture :: Texture -> IO ()
saveTexture (Texture typ (w,h) tex) = do err <- GL.get GLU.errors
                                         forM_ err (putStrLn . ("a"++) . show)
                                         old <- GL.get $ GL.textureBinding target
                                         GL.textureBinding target GL.$= Just tex
                                         allocaArray (w*h) (\ptr -> do
                                           GL.getTexImage (Left target) 0 
                                                 (GL.PixelData GL.RGBA GL.UnsignedByte (ptr :: Ptr Word32))
                                           -- writeImageFromPtr "texture" (h,w) ptr
                                           point <- peekArray (w*h) ptr
                                           image <- newImage (w,h)
                                           let col w = ( (fromIntegral $ (w .&. 0xFF000000) `shiftR` 24)
                                                       , (fromIntegral $ (w .&. 0x00FF0000) `shiftR` 16)
                                                       , (fromIntegral $ (w .&. 0x0000FF00) `shiftR` 8) 
                                                       , (fromIntegral $ (w .&. 0x000000FF)) )
                                               c (a,b,c,d) = rgba d c b (255 - a)
                                           mapM_ (\(k,v) -> setPixel (k `mod` w, k `div` w) (c (col v)) image)
                                                 (zip [0..w*h-1] point)
                                           saveJpegFile (-1) "texture.jpg" image
                                           GL.get GLU.errors >>= mapM_ (putStrLn . show)
                                           )
                                         GL.textureBinding target GL.$= old
    where target = glTextureType typ
--}
