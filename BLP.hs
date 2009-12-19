module BLP where

import qualified Data.ByteString.Lazy as BS
import Text.Printf
import Control.Exception
import Data.Binary
import Data.Array
import Control.Monad(ap,liftM3)
import Utils

data CTYPE = DXT1
           | DXT3
           | DXT5
           deriving Show
data BLP   = BLPp{ blp_palette_    :: Array Int Int
                 , blp_size_       :: (Int,Int)
                 , blp_alpha_bits_ :: Int
                 , blp_raw_        :: [BS.ByteString] 
                 }
           | BLPc{ blp_type_ :: CTYPE
                 , blp_size_ :: (Int,Int)
                 , blp_raw_  :: [BS.ByteString]
                 }

data Header   = Header{ id_     :: String
                      , typ_    :: Int
                      , attr_   :: (Int,Int,Int)
                      , width_  :: Int
                      , height_ :: Int
                      , offset_ :: [Int]
                      , sizes_  :: [Int]
                      }

newBLP :: FilePath -> IO BLP
newBLP fpath = do 
  archive <- BS.readFile fpath
  let hdr = decode archive :: Header
  assert (id_ hdr == "BLP2" && typ_ hdr == 1) (return ())
  let sz       = (width_ hdr, height_ hdr)
  let palette  = listArray (0,255) $ getBunchOf 256 getUInt (BS.drop 148 archive)
  let dat base = map (\(o,s) -> BS.take (fromIntegral s) (BS.drop (fromIntegral o) base)) $ 
                 filter (\(o,s) -> o>0&&s>0) $ 
                 zip (offset_ hdr) (sizes_ hdr)
  case (attr_ hdr) of
    (1,0,_) -> return $ BLPp palette sz 0 (dat (BS.drop 1172 archive))
    (1,1,_) -> return $ BLPp palette sz 1 (dat (BS.drop 1172 archive))
    (1,8,_) -> return $ BLPp palette sz 8 (dat (BS.drop 1172 archive))
    (2,0,_) -> return $ BLPc DXT1 sz (dat (BS.drop 148 archive))
    (2,1,_) -> return $ BLPc DXT1 sz (dat (BS.drop 148 archive))
    (2,4,1) -> return $ BLPc DXT3 sz (dat (BS.drop 148 archive))
    (2,8,1) -> return $ BLPc DXT3 sz (dat (BS.drop 148 archive))
    (2,8,7) -> return $ BLPc DXT5 sz (dat (BS.drop 148 archive))
    (a,b,c) -> assert False (do putStrLn $ printf "Unknown blp format:%d,%d,%d" a b c 
                                return undefined)

instance Binary Header where
    get   = return Header `ap` (sequence [getCChar,getCChar,getCChar,getCChar])
                          `ap` getUInt
                          `ap` liftM3 (,,) getUChar getUChar getUChar
                          `ap` getUInt `ap` getUInt
                          `ap` (sequence $ replicate 16 getUInt)
                          `ap` (sequence $ replicate 16 getUInt)
    put _ = error "Cannot save BLP Header"