{-# LANGUAGE FlexibleContexts #-}
module Data.WOW.GL.ResourceLoader(glResourceLoader) where

import Control.Monad.Trans

import Data.WOW.World
import Data.WOW.FileSystem
import Data.WOW.M2Model
import Data.WOW.BLP
import Data.WOW.GL.Types
import Data.WOW.GL.Mesh
import Data.WOW.GL.Texture

glResourceLoader :: (M2World m f GLResource) => [ResourceLoader m GLResource]
glResourceLoader = [ ResourceLoader (flip checkExt ".m2")
                                    (\fp -> do fs <- getWorldField filesystem
                                               x  <- liftIO (openM2 fs fp)
                                               y  <- newMesh x
                                               return $ GLModel x y)
                   , ResourceLoader (flip checkExt ".blp") 
                                    (\fp -> do fs      <- getWorldField filesystem 
                                               Just ct <- liftIO $ findFile fs fp 
                                               tx      <- liftIO $
                                                 newTextureFromBLP tex_type_2d (openBLPfromByteString ct)
                                               return $ GLTexture tx) 
                   ]
