{-# LANGUAGE TemplateHaskell, ExistentialQuantification #-}
module Data.WOW.Resource where

import Data.Record.Label
import qualified Data.Map as M
import Control.Monad.State
import Control.Exception
import Control.Category((>>>))
import {-# SOURCE #-}Data.WOW.World


{--
class FileSystem fs where
    findFile :: fs -> FilePath -> IO BS.ByteString

type ResourceId = String

type ResourceLibrary = M.Map ResourceId

class Resource rs where
    new :: FileSystem fs => BS.ByteString -> World fs rs rs

data (FileSystem fs, Resource rs) => 
    WorldState fs rs = WorldState{ _fileSystem :: fs
                                 , _resLibrary :: ResourceLibrary rs }

type World a b = StateT (WorldState a b) IO 

fileSystem :: (FileSystem fs, Resource rs) => (WorldState fs rs) :-> fs
fileSystem = label _fileSystem setter
    where
      setter :: (FileSystem fs, Resource rs) => fs -> WorldState fs rs -> WorldState fs rs
      setter n w = w{ _fileSystem = n }

resLibrary :: (FileSystem fs, Resource rs) => (WorldState fs rs) :-> ResourceLibrary rs
resLibrary = label _resLibrary setter
    where
      setter :: (FileSystem fs, Resource rs) => ResourceLibrary rs -> WorldState fs rs -> WorldState fs rs
      setter n w = w{ _resLibrary = n }

loadResource :: (FileSystem fs, Resource rs) => ResourceId -> World fs rs rs
loadResource fpath = do chk <- findResource fpath
                        case chk of 
                          -- resource is already loaded
                          Just x  -> return x
                          -- load the resource and add it to library
                          Nothing -> do content <- getM fileSystem >>= lift . flip findFile fpath
                                        res     <- new content
                                        modM resLibrary (M.insert fpath res)
                                        return res

findResource :: (FileSystem fs, Resource rs) => ResourceId -> World fs rs (Maybe rs)
findResource id = do lib <- getM resLibrary
                     return $ M.lookup id lib

data MockMPQ = MockMPQ
instance FileSystem MockMPQ where
    findFile _ fpath | isJust (match mpq fpath [])
                         = let rfile = U.joinPath $ (prefix ++) $ map lower $ W.splitDirectories fpath
                           in  BS.readFile rfile
                     | otherwise 
                         = assert False (return BS.empty)
        where
          mpq    = compile "^MPQ:\\w+(\\\\\\w+)*$" [no_auto_capture]
          prefix = ["..", "tmp"]
          lower  = map toLower
--}