module Data.WOW.GL.Resource where

import Data.WOW.M2Model(M2Model)
import Data.WOW.GL.Types

data GLResource = GLModel M2Model Mesh
                | GLTexture Texture