module Main where

import Graphics.GD
import Codec.Image.STB as STB
import Data.Bits
import Text.Printf
import Foreign.Ptr
import Foreign.Marshal.Array
import Data.Word
import Codec.Image.DDS
import qualified Data.ByteString as BS
import Data.ByteString.Internal
import Foreign.Marshal.Utils
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc

import Data.WOW.BLP

{--
main = do Right image <- loadImage "../tmp/world/goober/mm_xmas_treebranch_01.png"
          STB.withImage image (\ptr (w,h) chn -> do
                       putStrLn (printf "w:%d,h:%d,c:%d" (w :: Int) (h :: Int) (chn :: Int))
                       point <- peekArray (w*h) (castPtr ptr :: Ptr Word32)
                       ni <- newImage (w,h)
                       let col w = ( (fromIntegral $ (w .&. 0xFF000000) `shiftR` 24)
                                   , (fromIntegral $ (w .&. 0x00FF0000) `shiftR` 16)
                                   , (fromIntegral $ (w .&. 0x0000FF00) `shiftR` 8) 
                                   , (fromIntegral $ (w .&. 0x000000FF)) )
                           c (a,b,c,d) = rgba d c b (255 - a)
                       mapM_ (\(k,v) -> setPixel (k `mod` w, k `div` w) (c (col v)) ni)
                             (zip [0..w*h-1] point)
                       saveJpegFile (-1) "right.jpg" ni )
--}

b0 = "MPQ:World\\Goober\\xmastreelarge_alliance01star.blp"
b1 = "MPQ:Creature\\Chicken\\ChickenSkinBrown.blp"
b1'= "FILE:../tmp/ChickenSkinBrown.blp"
b2 = "MPQ:CREATURE\\DRAGON\\ONYXIAMOUNT3.BLP"

main = do x <- newBLP b2
          let ctype = blp_type_ x
              (w,h) = blp_size_ x
              raw   = head $ blp_raw_  x
              t'    = case ctype of DXT1' -> DXT1
                                    DXT3' -> DXT3
                                    DXT5' -> DXT5

          putStrLn $ "Decompressing.." ++ show ctype
          BS.writeFile "in" raw

          withDecompressed t' (BS.unpack raw) w h
                           (\pout -> do let l = w*h*4 
                                        putStrLn (printf "%d %d %d" w h l)
                                        b <- create l (\dst -> copyBytes dst pout l)
                                        BS.writeFile "out" b
                                        saveImage "texture.png" w h (castPtr pout :: Ptr Word32))

saveImage file w h ptr = do
  point <- peekArray (w*h) ptr :: IO [Word32]
  image <- newImage (w,h)
  let col w = ( (fromIntegral $ (w .&. 0xFF000000) `shiftR` 24)
              , (fromIntegral $ (w .&. 0x00FF0000) `shiftR` 16)
              , (fromIntegral $ (w .&. 0x0000FF00) `shiftR` 8) 
              , (fromIntegral $ (w .&. 0x000000FF)) )
      c (a,b,c,d) = rgba d c b (255 - a)
  mapM_ (\(k,v) -> setPixel (k `mod` w, k `div` w) (c (col v)) image)
        (zip [0..w*h-1] point)
  saveJpegFile (-1) file image

