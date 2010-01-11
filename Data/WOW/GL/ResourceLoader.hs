module Data.WOW.GL.ResourceLoader(glResourceLoader) where

import Control.Monad.Trans(lift)
import Data.Record.Label
import Data.Char

import Data.WOW.World
import Data.WOW.FileSystem
import Data.WOW.M2Model
import Data.WOW.BLP
import Data.WOW.GL.Types
import Data.WOW.GL.Mesh
import Data.WOW.GL.Texture

glResourceLoader :: (FileSystem f) => [ResourceLoader f GLResource]
glResourceLoader = [ ResourceLoader (flip checkExt ".m2")
                                    (\fp -> do fs <- getM filesystem
                                               x <- lift $ openM2 fs fp
                                               y <- newMesh x
                                               return $ GLModel x y)
                   , ResourceLoader (flip checkExt ".blp") 
                                    (\fp -> do fs <- getM filesystem 
                                               Just ct <- lift $ findFile fs fp 
                                               tx <- lift $ newTextureFromBLP TEX_TYPE_2D (openBLPfromByteString ct)
                                               return $ GLTexture tx) 
                   ]