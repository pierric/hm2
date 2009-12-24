module GL.ResourceLoader where

import Control.Monad.Trans(lift)
import Data.Char

import FileSystem
import Resource
import M2Model
import BLP
import GL.Types
import GL.Mesh
import GL.Texture
import GL.Resource

import Debug.Trace
import qualified System.FilePath.Windows as W
import System.FilePath

glResourceLoader = [ ResourceLoader ((== ".m2") . lower . extension )
                                    (\fp -> lift (newModel fp) >>= newMesh    >>= return . GLMesh)
{--
                   , ResourceLoader ((== ".blp") . lower . extension)
                                    (\fp -> let f' = ("FILE:" ++) $ lower $ flip replaceExtension "png" $
                                                     joinPath $ ["..","tmp"] ++ W.splitDirectories (drop 4 fp)
                                            in  trace ("load " ++ f' ++" instead of " ++ fp) $ 
                                                lift $ newTexture f' >>= return . GLTexture) ]
--}

                   , ResourceLoader ((== ".blp") . lower . extension ) 
                                    (\fp -> lift $ newBLP fp >>= newTextureFromBLP TEX_TYPE_2D >>= return . GLTexture) ]

    where lower = map toLower