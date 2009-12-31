module Data.WOW.GL.Types where

import Data.Word
import qualified Graphics.Rendering.OpenGL.GL as GL
import Data.Tensor

import Data.WOW.M2Model(RenderPass)
import Data.WOW.BLP(CTYPE(..))
import Data.WOW.World(ResourceId)

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
                , renderpasses_ :: [RenderPass]
                , submeshes_    :: [SubMesh]
                , textures_     :: [ResourceId]
                -- each vertex can be affected by 4 bones
                -- an array of (bone_index, weight) for each vertex
                , bone_weight_  :: [(BoneWeight,BoneWeight,BoneWeight,BoneWeight)]
                }

data Texture = Texture{ -- tx_name_   :: String
                        tx_type_   :: TextureType
                      , tx_size_   :: (Int,Int)
                      , tx_object_ :: GL.TextureObject 
                      }

data TextureType = TEX_TYPE_1D
                 | TEX_TYPE_2D
                 | TEX_TYPE_3D
                 | TEX_TYPE_CUBE_MAP

glTextureType TEX_TYPE_1D = GL.Texture1D
glTextureType TEX_TYPE_2D = GL.Texture2D
glTextureType TEX_TYPE_3D = GL.Texture3D
glTextureType TEX_TYPE_CUBE_MAP = GL.TextureCubeMap