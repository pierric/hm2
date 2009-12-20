{-# LANGUAGE TypeFamilies #-}
module Resource where

import Text.Regex.PCRE.Light.Char8
import qualified System.FilePath.Windows as W
import qualified System.FilePath.Unix as U

type ResourceId = String
newtype Resource a => ResourceLibrary a = ResourceLibrary (M.Map ResourceId a)

class Resource a where
    mkResource :: FileSystem fs => fs -> IO a

loadResource :: (FileSystem f, Resource a) => f -> ResourceLibrary a -> ResourceId -> IO (a, ResourceLibrary a)
loadResource fs res fpath = mkResource fs

findResource :: ResourceLibrary a -> ResourceId -> Maybe a
findResource =  flip M.lookup

class FileSystem a where
    findFile :: FilePath -> IO BS.ByteString

data MockMPQ = MockMPQ

instance FileSystem MockMPQ where
    findFile fpath =  when (match mpq fpath)
                           (let rfile = U.joinPath $ (prefix ++) $ map lower $ W.splitDirectory fpath
                            in  BS.readFile rfile)
    where
      mpq    = compile "^MPQ:\\w+(\\\\\\w+)*$" [no_auto_capture]
      prefix = ["..", "tmp"]

{--
class Monad m => ResourceM m where
    data ResourceItem m :: *
    loadResource :: ResourceId -> m ()
    findResource :: ResourceId -> m (ResourceItem m)

newtype GLM a = GLM{ runGLM :: [ResourceItem GLM] -> (a, [ResourceItem GLM]) }
instance Monad GLM where 
    return c = GLM (\r -> (c,r))
    a >>= f  = GLM (\r -> let (b,r') = runGLM a r
                          in  runGLM (f b) r')
    
instance ResourceM GLM where
    data ResourceItem GLM = Texture 
                          | Mesh
    loadResource id = 
--}