module Data.WOW.BLP where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as SBS
import Text.Printf
import Control.Exception
import Data.Binary
import Data.Array
import Data.Binary.Get
import Data.Word
import Control.Monad(ap,liftM3)

import Data.WOW.FileSystem
import Data.WOW.Utils

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
  Just archive <- findFile fpath
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