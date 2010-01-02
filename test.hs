module Main where

import Data.Word
import Data.Bits
import Codec.Image.DDS
import Data.Binary
import Data.Binary.Get
import Data.Array
import Text.Printf
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as SBS
import Data.ByteString.Internal
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Graphics.GD
import Control.Monad
import Control.Exception
import Data.WOW.BLP
{--
main = do 
  content <- BS.readFile "tmp/ChickenSkinBrown.blp"
  let (typ, [a0,a1,a2,a3], widt, heit, off ,siz)
          = flip runGet (content <+> 4)
                        (do typ  <- getWord32le
                            attr <- sequence $ replicate 4  getWord8
                            widt <- getWord32le
                            heit <- getWord32le
                            off  <- sequence $ replicate 16 getWord32le
                            siz  <- sequence $ replicate 16 getWord32le
                            return (typ, attr, widt, heit, off, siz) )
  putStrLn $ printf "BLP:type[%d] attr[%d,%d,%d,%d] size[%d,%d]" typ a0 a1 a2 a3 widt heit
  let dat = BS.take (fromIntegral $ head siz) (content <+> head off)
  output <- mallocArray (fromIntegral $ widt * heit) :: IO (Ptr Word32)
  withArray (BS.unpack dat) (\ptr -> ddsDecompressDXT3 ptr (fromIntegral widt) (fromIntegral heit) (castPtr output :: Ptr Word8))
  saveImage "hsOut" (fromIntegral widt) (fromIntegral heit) output
  free output

bs <+> offset = BS.drop (fromIntegral offset) bs

--}
{--
data CTYPE = DXT1'
           | DXT3'
           | DXT5'
           deriving Show
data BLP   = BLPp{ blp_name        :: String
                 , blp_palette_    :: Array Int Word32
                 , blp_size_       :: (Int,Int)
                 , blp_alpha_bits_ :: Int
                 , blp_raw_        :: [SBS.ByteString] 
                 }
           | BLPc{ blp_name        :: String
                 , blp_type_       :: CTYPE
                 , blp_size_       :: (Int,Int)
                 , blp_raw_        :: [SBS.ByteString]
                 }

data Header   = Header{ id_     :: String
                      , typ_    :: Int
                      , attr_   :: (Int,Int,Int)
                      , mipmap_ :: Bool
                      , width_  :: Int
                      , height_ :: Int
                      , offset_ :: [Int]
                      , sizes_  :: [Int]
                      }

newBLP :: FilePath -> IO BLP
newBLP fpath = do 
  archive <- BS.readFile fpath
  let hdr      = decode archive :: Header
      sz       = (width_ hdr, height_ hdr)
      palette  = listArray (0,255) $ getBunchOf 256 getWord32le (BS.drop 148 archive)
      dat      = map (\(o,s) -> SBS.copy $ SBS.concat $ BS.toChunks $ 
                                BS.take (fromIntegral s) $ BS.drop (fromIntegral o) archive)
                     (filter (\(o,s) -> o>0&&s>0) $ zip (offset_ hdr) (sizes_ hdr))
  assert (id_ hdr == "BLP2" && typ_ hdr == 1) (return ())
  return $ case (attr_ hdr) of
             (1,0,_) -> BLPp fpath palette sz 0 dat
             (1,1,_) -> BLPp fpath palette sz 1 dat
             (1,8,_) -> BLPp fpath palette sz 8 dat
             (2,0,_) -> BLPc fpath DXT1' sz dat
             (2,1,_) -> BLPc fpath DXT1' sz dat
             (2,4,1) -> BLPc fpath DXT3' sz dat
             (2,8,1) -> BLPc fpath DXT3' sz dat
             (2,8,7) -> BLPc fpath DXT5' sz dat
             (a,b,c) -> assert False undefined 

instance Binary Header where
    get   = return Header `ap` (sequence [getCChar,getCChar,getCChar,getCChar])
                          `ap` getUInt
                          `ap` liftM3 (,,) getUChar getUChar getUChar
                          `ap` (fmap (>0) getUChar)
                          `ap` getUInt `ap` getUInt
                          `ap` (sequence $ replicate 16 getUInt)
                          `ap` (sequence $ replicate 16 getUInt)
    put _ = error "Cannot save BLP Header"

getCChar  :: Get Char
getCChar  = get
getUChar  :: Integral i => Get i
getUChar  = fmap fromIntegral getWord8
getUInt   :: Integral i => Get i
getUInt   = fmap fromIntegral getWord32le
getBunchOf :: Int -> Get a -> BS.ByteString -> [a]
getBunchOf cnt g content = runGet (sequence $ replicate cnt g) content
--}

main = do x <- newBLP "FILE:../tmp/creature/dragon/onyxiamount3.blp"
          let ctype = blp_type_ x
              (w,h) = blp_size_ x
              raw   = head $ blp_raw_  x
              t'    = case ctype of DXT1' -> DXT1
                                    DXT3' -> DXT3
                                    DXT5' -> DXT5

          putStrLn $ "Decompressing.." ++ show ctype

          let dcomp  = case t' of
                         DXT1 -> ddsDecompressDXT1
                         DXT3 -> ddsDecompressDXT3
                         DXT5 -> ddsDecompressDXT5
          allocaBytes (w * h * 4)
                      (\pout -> do 
                         withArray (SBS.unpack raw) (\pin -> dcomp pin w h pout)
                         let l = w*h*4 
                         putStrLn (printf "%d %d %d" w h l)
                         b <- create l (\dst -> copyBytes dst pout l)
                         SBS.writeFile "out" b
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

