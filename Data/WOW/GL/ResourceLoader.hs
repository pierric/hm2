module Data.WOW.GL.ResourceLoader where

import Control.Monad.Trans(lift)
import Data.Char

import Data.WOW.World
import Data.WOW.FileSystem
import Data.WOW.M2Model
import Data.WOW.BLP
import Data.WOW.GL.Types
import Data.WOW.GL.Mesh
import Data.WOW.GL.Texture
import Data.WOW.GL.Resource

import Debug.Trace
import qualified System.FilePath.Windows as W
import System.FilePath

glResourceLoader = [ ResourceLoader (flip checkExt ".m2")
                                    (\fp -> do x <- lift (newModel fp) 
                                               y <- newMesh x
                                               return $ GLModel x y)
{--
                   , ResourceLoader ((== ".blp") . lower . extension)
                                    (\fp -> let f' = ("FILE:" ++) $ lower $ flip replaceExtension "png" $
                                                     joinPath $ ["..","tmp"] ++ W.splitDirectories (drop 4 fp)
                                            in  trace ("load " ++ f' ++" instead of " ++ fp) $ 
                                                lift $ newTexture f' >>= return . GLTexture) ]
--}
                   , ResourceLoader (flip checkExt ".blp") 
                                    (\fp -> lift $ newBLP fp >>= newTextureFromBLP TEX_TYPE_2D >>= return . GLTexture) ]

    where lower = map toLower