module Data.WOW.GL.Mesh where

import Data.Word
import Data.Tensor
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Control.Exception
import Control.Monad(when)
import Control.Monad.Trans(lift)
import Data.VectorSpace((*^), sumV)
import Debug.Trace

import Data.WOW.World
import Data.WOW.M2Model
import Data.WOW.ModelDef
import Data.WOW.GL.Types
import Data.WOW.GL.Resource
import Data.WOW.Matrix
import Data.WOW.Utils

newMesh :: M2Model -> World GLResource Mesh
newMesh mdl = do mapM_ findResource tex
                 lift $ do [vbo] <- GL.genObjectNames 1
                           GL.bindBuffer GL.ArrayBuffer GL.$= Just vbo
                           -- alloca buffer data
                           -- 16 bytes for position
                           -- 12 bytes for normal
                           -- 8  bytes for texcoord
                           let len = length (m_vertices_ mdl)
                           GL.bufferData GL.ArrayBuffer GL.$= (fromIntegral $ len * 36, nullPtr, GL.StaticDraw)
                           GL.withMappedBuffer GL.ArrayBuffer GL.WriteOnly
                                 (\ptr -> let pos = map (to4 . fix3 . vd_pos_) (m_vertices_ mdl)
                                              nor = map (fix3 . vd_normal_)    (m_vertices_ mdl)
                                              tcd = map vd_texcoords_         (m_vertices_ mdl)
                                          in  do withArrayLen pos (\l s -> copyArray ptr   s l)
                                                 let ptr'  = ptr `plusPtr` (len * 16)
                                                 withArrayLen nor (\l s -> copyArray ptr'  s l)
                                                 let ptr'' = ptr `plusPtr` (len * 28)
                                                 withArrayLen tcd (\l s -> copyArray ptr'' s l))
                                 (\err -> assert False (return ()))
                           let sm = map (\gs -> SubMesh (mg_vstart_ gs) (mg_vcount_ gs)
                                                        (mg_istart_ gs) (mg_icount_ gs)
                                        ) (m_geoset_ mdl)
                           return $ Mesh{ vertices_ = (len,vbo)
                                        , indices_  = map fromIntegral (m_indices_ mdl)
                                        , renderpasses_ = m_renderpass_ mdl
                                        , submeshes_    = sm
                                        , textures_     = tex
                                        , bone_weight_  = map bw (m_vertices_ mdl) }
    where
      tex = flip map (m_textures_ mdl) (\t -> case t of
                                                Data.WOW.M2Model.Texture f _ -> "MPQ:" ++ f
                                                _ -> "Unknown")
      bw vert = case zip (map fromIntegral $ vd_bones_ vert) (map fromIntegral $ vd_weights_ vert) of
                  [x,y,z,w] -> (x,y,z,w)

skeletonAnim :: [Matrix] -> Mesh -> World res Mesh
skeletonAnim bones mesh = 
    lift $ do GL.withMappedBuffer GL.ArrayBuffer GL.ReadWrite 
                    (\ptr -> do let num = fst $ vertices_ mesh
                                vert <- peekArray num (castPtr ptr :: Ptr (Vector4 Float))
                                withArrayLen (map (uncurry sumbw) $ zip vert (bone_weight_ mesh))
                                             (\l s -> assert (l == num) (copyArray ptr s l))
                                return mesh)
                    (\err -> assert False (return mesh))
    where
      sumbw :: Vector4 Float -> (BoneWeight,BoneWeight,BoneWeight,BoneWeight) -> Vector4 Float
      sumbw (Vector4 a b c d) (w1,w2,w3,w4)
          = fix4 $ sumV $ map (\w-> let m = if fromIntegral (fst w) < length bones
                                            then bones !! fromIntegral (fst w)
                                            else trace (show w ++ ";" ++ show (length bones)) $ undefined
                                        e = fromIntegral (snd w) / 255.0
                                    in  e *^ (m `multVec4` (Vector4 a (-c) b d))
                              ) $ [w1,w2,w3,w4]

drawSubMesh mesh idx = 
    let sm  = submeshes_ mesh !! idx
        rng = (fromIntegral $ sm_vstart_ sm, fromIntegral $ sm_vstart_ sm + sm_vcount_ sm)
        (len, vbo) = vertices_ mesh
        (is,ic) = (sm_istart_ sm, sm_icount_ sm)
    in  do GL.bindBuffer   GL.ArrayBuffer GL.$= Just vbo
           GL.clientState  GL.VertexArray GL.$= GL.Enabled
           GL.arrayPointer GL.VertexArray GL.$= 
                   GL.VertexArrayDescriptor 4 GL.Float 0 (intPtrToPtr 0 :: Ptr GL.GLint)
           GL.clientState  GL.NormalArray GL.$= GL.Enabled
           GL.arrayPointer GL.NormalArray GL.$= 
                   GL.VertexArrayDescriptor 3 GL.Float 0 (intPtrToPtr (fromIntegral $ len * 16) :: Ptr ())
           GL.clientState  GL.TextureCoordArray GL.$= GL.Enabled
           GL.arrayPointer GL.TextureCoordArray GL.$= 
                   GL.VertexArrayDescriptor 2 GL.Float 0 (intPtrToPtr (fromIntegral $ len * 28) :: Ptr ())
           withArray (take ic $ drop is $ indices_ mesh) 
                     (GL.drawRangeElements GL.Triangles rng (fromIntegral ic) GL.UnsignedShort)
           GL.clientState GL.VertexArray       GL.$= GL.Disabled
           GL.clientState GL.NormalArray       GL.$= GL.Disabled
           GL.clientState GL.TextureCoordArray GL.$= GL.Disabled

withMaterial mesh rp act = 
    do mb <- findResource (textures_ mesh !! r_tex_ rp)
       case mb of 
         Nothing -> 
             do lift $ do GL.blend GL.$= GL.Disabled
                          GL.alphaFunc GL.$= Nothing
                          GL.textureBinding GL.Texture2D GL.$= Nothing
                          act
         Just (GLTexture tex) -> 
             lift $ do case r_blendmode_ rp of
                         0 -> return ()
                         1 -> GL.alphaFunc    GL.$= Just (GL.Gequal, 0.7)
                         2 -> do GL.blend     GL.$= GL.Enabled 
                                 GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
                         3 -> do GL.blend     GL.$= GL.Enabled
                                 GL.blendFunc GL.$= (GL.SrcColor, GL.One)
                         4 -> do GL.blend     GL.$= GL.Enabled
                                 GL.blendFunc GL.$= (GL.SrcAlpha, GL.One)
                         5 -> do GL.blend     GL.$= GL.Enabled
                                 GL.blendFunc GL.$= (GL.DstColor, GL.SrcColor)
                         6 -> do GL.blend     GL.$= GL.Enabled
                                 GL.blendFunc GL.$= (GL.DstColor, GL.SrcColor)
                         _ -> do putStrLn "Unknown blendmode" 
                                 assert False (return ())
                       when (r_swrap_ rp) (GL.textureWrapMode GL.Texture2D GL.S GL.$= (GL.Repeated, GL.Repeat))
                       when (r_twrap_ rp) (GL.textureWrapMode GL.Texture2D GL.T GL.$= (GL.Repeated, GL.Repeat))
                       when (r_noZWrite_ rp) (GL.depthMask GL.$= GL.Disabled)
                       when (r_useEnvMap_ rp) (putStrLn "requires environment map")
                       when (r_texanim_ rp>=0) (putStrLn "requires texture animation")
                       when (r_unlit_ rp) (GL.lighting GL.$= GL.Disabled)
                       GL.textureBinding (glTextureType (tx_type_ tex)) GL.$= Just (tx_object_ tex)
                       act
                       GL.blend     GL.$= GL.Disabled
                       GL.alphaFunc GL.$= Nothing
                       when (r_noZWrite_ rp) (GL.depthMask GL.$= GL.Enabled)
                       when (r_unlit_ rp)    (GL.lighting  GL.$= GL.Enabled)
                       when (r_cull_ rp)     (GL.cullFace  GL.$= Nothing)
                       when (r_swrap_ rp)
                            (GL.textureWrapMode GL.Texture2D GL.S GL.$= (GL.Repeated, GL.ClampToEdge))
                       when (r_twrap_ rp)
                            (GL.textureWrapMode GL.Texture2D GL.T GL.$= (GL.Repeated, GL.ClampToEdge))
                                 

-- renderAll is for test purpose. It simplely renders every submesh
renderAll :: Mesh -> World GLResource ()
renderAll mesh = -- (\r -> withMaterial mesh r (drawSubMesh mesh (r_geoset_ r))) (renderpasses_ mesh !! 1)
                 mapM_ (\r -> withMaterial mesh r (drawSubMesh mesh (r_geoset_ r))) (renderpasses_ mesh)
