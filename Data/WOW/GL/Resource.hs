module Data.WOW.GL.Resource where

import Data.WOW.GL.Types

data GLResource = GLMesh Mesh
                | GLTexture Texture