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

glResourceLoader = [ ResourceLoader ((== ".m2") . lower . extension )
                                  (\fp -> lift (newModel fp) >>= newMesh    >>= return . GLMesh)
                 , ResourceLoader ((== ".blp") . lower . extension ) 
                                  (\fp -> lift $ newBLP fp >>= newTexture TEX_TYPE_2D >>= return . GLTexture) ]
    where lower = map toLower