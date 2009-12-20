module Mesh where

data SubMesh = SubMesh{ sm_vstart_ 
                      , sm_vcount_
                      , sm_istart_
                      , sm_icount_
                      }


data Mesh = Mesh{ vertices_ :: VertexData
                , indices_  :: IndexData
                , renderpasses_ :: [RenderPass]
                , submeshes_    :: [SubMesh]
                }

newMesh :: M2Model -> IO Mesh
newMesh mdl = ..


renderMesh :: Mesh -> IO ()
renderMesh mesh = mapM_ (\r -> withMaterial r (draw (r_geoset_ r))) (renderpasses_ mesh)
    where 
      draw idx = let sm  = submeshes mesh !! idx
                     rng = (sm_vstart_ sm, sm_vstart_ sm + sm_vcount_ sm)
                 in GL.drawRangeElements GL.Triangles rng (sm_icount sm) GL.UnsignedShort ({-- point to indices --})