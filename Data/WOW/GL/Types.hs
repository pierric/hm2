module Data.WOW.GL.Types where

import Data.Word
import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GL.Tensor

import qualified Data.WOW.M2Model as M2
import Data.WOW.BLP(CTYPE(..))
import Data.WOW.World(ResourceId)

data GLResource = GLModel M2.M2Model Mesh
                | GLTexture Texture

data SubMesh = SubMesh{ sm_vstart_ 
                      , sm_vcount_
                      , sm_istart_
                      , sm_icount_ :: Int
                      }

type BoneWeight = (Word8, Word8)

data Mesh = Mesh{ vertices_     :: (Int,GL.BufferObject)
                , orig_vert_    :: [Vector3 Float]
                , orig_norm_    :: [Vector3 Float]
                , indices_      :: [Word16]
                , renderpasses_ :: [M2.RenderPass]
                , submeshes_    :: [SubMesh]
                , textures_     :: [M2.Texture]
                -- each vertex can be affected by 4 bones
                -- an array of (bone_index, weight) for each vertex
                , bone_weight_  :: [(BoneWeight,BoneWeight,BoneWeight,BoneWeight)]
                }

data Texture   = forall t. (GL.BindableTextureTarget t) =>
                 Texture{ -- tx_name_   :: String
                          tx_type_   :: t
                        , tx_object_ :: GL.TextureObject 
                        }

tex_type_1d       = GL.Texture1D
tex_type_2d       = GL.Texture2D
tex_type_3d       = GL.Texture3D
tex_type_cube_map = GL.TextureCubeMap

