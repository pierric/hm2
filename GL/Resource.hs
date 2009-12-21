module GL.Resource where

import Resource
import GL.Types

data GLResource = GLMesh Mesh
                | GLTexture Texture