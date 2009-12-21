module GL.Mesh where

import Data.Word
import Data.Tensor
import qualified Graphics.Rendering.OpenGL.GL as GL
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Control.Exception
import Control.Monad.Trans(lift)

import Resource
import M2Model
import ModelDef
import GL.Types
import GL.Texture
import GL.Resource

newMesh :: M2Model -> World GLResource Mesh
newMesh mdl = do mapM_ loadResource tex
                 lift $ do [vbo] <- GL.genObjectNames 1
                           GL.bindBuffer GL.ArrayBuffer GL.$= Just vbo
                           -- alloca buffer data
                           -- 16 bytes for position
                           -- 12 bytes for normal
                           -- 8  bytes for texcoord
                           let len = length (m_vertices_ mdl)
                           GL.bufferData GL.ArrayBuffer GL.$= (fromIntegral $ len * 36, nullPtr, GL.StaticDraw)
                           GL.withMappedBuffer GL.ArrayBuffer GL.WriteOnly	
                                 (\ptr -> let pos = map (to4 . fix . vd_pos_) (m_vertices_ mdl)
                                              nor = map (fix . vd_normal_)    (m_vertices_ mdl)
                                              tcd = map vd_texcoords_         (m_vertices_ mdl)
                                          in  do withArrayLen pos (\l s -> copyArray ptr   s l)
                                                 let ptr'  = ptr `plusPtr` (len * 16)
                                                 withArrayLen nor (\l s -> copyArray ptr'  s l)
                                                 let ptr'' = ptr `plusPtr` (len * 28)
                                                 withArrayLen tcd (\l s -> copyArray ptr'' s l))
                                 (\err -> assert False $ return ())
                           let sm = map (\gs -> SubMesh (mg_vstart_ gs) (mg_vcount_ gs)
                                                        (mg_istart_ gs) (mg_icount_ gs)
                                        ) (m_geoset_ mdl)
                           return $ Mesh (len,vbo) (map fromIntegral (m_indices_ mdl)) (m_renderpass_ mdl) sm tex
    where
      fix (Vector3 x y z) = Vector3 x z (-y)
      to4 (Vector3 x y z) = Vector4 x y z 1
      tex = map fromFT (m_textures_ mdl)
      fromFT (M2Model.Texture f _) = "MPQ:" ++ f

renderMesh :: Mesh -> World GLResource ()
renderMesh mesh = mapM_ (\r -> withMaterial r (draw (r_geoset_ r))) (renderpasses_ mesh)
    where 
      draw idx = let sm  = submeshes_ mesh !! idx
                     rng = (fromIntegral $ sm_vstart_ sm, fromIntegral $ sm_vstart_ sm + sm_vcount_ sm)
                     (len, vbo) = vertices_ mesh
                 in  do GL.bindBuffer   GL.ArrayBuffer GL.$= Just vbo
                        GL.clientState  GL.VertexArray GL.$= GL.Enabled
                        GL.arrayPointer GL.VertexArray GL.$=
                          GL.VertexArrayDescriptor 4 GL.Float 0 (intPtrToPtr 0 :: Ptr GL.GLint)
                        GL.arrayPointer GL.NormalArray GL.$= 
                          GL.VertexArrayDescriptor 3 GL.Float 0 (intPtrToPtr (fromIntegral $ len * 16) :: Ptr GL.GLint)
                        GL.arrayPointer GL.TextureCoordArray GL.$= 
                          GL.VertexArrayDescriptor 2 GL.Float 0 (intPtrToPtr (fromIntegral $ len * 28) :: Ptr GL.GLint)
                        withArray (indices_ mesh) 
                          (GL.drawRangeElements GL.Triangles rng (fromIntegral $ sm_icount_ sm) GL.UnsignedShort)
                        GL.clientState GL.VertexArray  GL.$= GL.Disabled

      withMaterial rp act = do Just (GLTexture tex) <- findResource (textures_ mesh !! r_tex_ rp)
                               lift $ withTexture tex act
