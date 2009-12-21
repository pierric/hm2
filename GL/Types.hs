module GL.Types where

import Data.Word
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLR

import M2Model(RenderPass)
import BLP(CTYPE(..))
import Resource(ResourceId)

data SubMesh = SubMesh{ sm_vstart_ 
                      , sm_vcount_
                      , sm_istart_
                      , sm_icount_ :: Int
                      }


data Mesh = Mesh{ vertices_ :: (Int,GL.BufferObject)
                , indices_  :: [Word16]
                , renderpasses_ :: [RenderPass]
                , submeshes_    :: [SubMesh]
                , textures_     :: [ResourceId]
                }

data Texture = Texture{ -- tx_name_   :: String
                        tx_type_   :: TextureType
                      , tx_object_ :: GL.TextureObject 
                      }

data TextureType = TEX_TYPE_1D
                 | TEX_TYPE_2D
                 | TEX_TYPE_3D
                 | TEX_TYPE_CUBE_MAP


withTexture tex act = do GL.textureBinding typ GL.$= Just (tx_object_ tex)
                         act
                         GL.textureBinding typ GL.$= Nothing
    where
      typ = glTextureType (tx_type_ tex)

glTextureType TEX_TYPE_1D = GL.Texture1D
glTextureType TEX_TYPE_2D = GL.Texture2D
glTextureType TEX_TYPE_3D = GL.Texture3D
glTextureType TEX_TYPE_CUBE_MAP = GL.TextureCubeMap

glCompressedTextureFormat DXT1 = GL.CompressedTextureFormat GLR.gl_COMPRESSED_RGBA_S3TC_DXT1 
glCompressedTextureFormat DXT3 = GL.CompressedTextureFormat GLR.gl_COMPRESSED_RGBA_S3TC_DXT3
glCompressedTextureFormat DXT5 = GL.CompressedTextureFormat GLR.gl_COMPRESSED_RGBA_S3TC_DXT5
