module M2Model where

import Control.Exception
import qualified ModelDef as MD
import Utils

data M2Model = M2Model{ m_name_ :: String
                      , m_global_sequence_ :: [Int]
                      , m_vertices_      :: [Vertex] -- vertices
                      , m_indices_       :: [Int]     -- triagnles by index
                      , m_textures_      :: [Texture]
                      , m_geoset_        :: [Geoset]
                      , n_renderpass_    :: [RenderPass]
                      , m_colors_        :: [(Animated Color, Animated Opacity)]
                      , m_transparency_  :: [Animated Short]
                      , m_trans_lookup_  :: [Int]
                      , m_attachments_   :: [Attachment]
                      , m_attach_lookup_ :: [Int]
                      }

type Vertex = MD.VertexDef

data Texture = Texture    { t_filename_ :: String, t_flags_ :: Int }
             | CharTexture{ t_type_     :: Int   , t_flags_ :: Int }

type Geoset = MD.Geoset

data RenderPass = RenderPass{ r_geoset_ :: Geoset
	                    , r_tex_    :: Texture
                            , r_indexStart_, r_indexCount_
                            , r_vertexStart_, r_vertexEnd_ :: Int
	                    , r_useTex2_, r_useEnvMap_, r_cull_, r_trans_
                            , r_unlit_, r_noZWrite_, r_billboard_ :: Bool
	                    , r_p_ :: Float
	                    , r_texanim_, r_color_, r_opacity_, r_blendmode_ :: Int
	                    , r_order_ :: Int
	                    , r_swrap_, r_twrap_ :: Bool
                            }

type Attachment = MD.AttachmentDef

build :: FilePath -> IO M2Model
build fpath = do archive <- BS.readFile fpath
                 let def  = decode archive :: MD.Header
                 assert (nViews_ def > 0) (return ())

                 -- read a few definitions
                 let bunchOf cnt offset g = getBunchOf cnt g (BS.drop offset archive)
                 let gseq = bunchOf (nGlobalSequences_ def)    (ofsGlobalSequences_ def) getUInt
                 let vert = bunchOf (nVertices_ def)           (ofsVertices_ def)        (get :: Get MD.VertexDef)
                 let txdf = bunchOf (nTextures_ def)           (ofsTextures_ def)        (get :: Get MD.TextureDef)
                 let atdf = bunchOf (nAttachments_ def)        (ofsAttachments_ def)     (get :: Get MD.AttachmentDef)
                 let atlk = bunchOf (nAttachLookup_ def)       (ofsAttachLookup_ def)    getUShort
                 let cldf = bunchOf (nColors_ def)             (ofsColors_ def)          (get :: Get MD.ColorDef)
                 let trdf = bunchOf (nTransparency_ def)       (ofsTransparency_ def)    (get :: MD.TransDef)
                 let trlk = bunchOf (nTransparencyLookup_ def) (ofsTransparencyLookup_ def) getWord16le
                 let rflg = bunchOf (nTexFlags_ def)           (ofsTexFlags_ def)        (get :: Get MD.RenderFlags)
                 let txlk = bunchOf (nTexLookup_ def)          (ofsTexLookup_ def)       getUShort
                 let talk = bunchOf (nTexAnimLookup_ def)      (ofsTexAnimLookup_ def)   getUShort
                 let tulk = bunchOf (nTexUnitLookup_ def)      (ofsTexUnitLookup_ def)   getUShort

                 -- textures
                 let textures = assert (nTextures_ def < 32) $ 
                                map (\def -> case td_type_ def of
                                               0 -> Texture (bunchOf (td_nameLen_ def)
                                                                     (td_nameOfs_ def) (get :: Get Char))
                                                            (td_flags_ def)
                                               _ -> 
                                    ) txdf

                 -- view
                 (indices, geoset, texunit) <- lod fpath

                 -- renderpasses
                 let rps  = map (renderpass textures geoset rflg trdf txlk talk tulk) texunit

                 -- create the model
                 return $ M2Model fpath
                                  gseq
                                  vert
                                  indices
                                  textures
                                  geoset
                                  (sort rps)
                                  ?? ?? 
                                  trlk 
                                  atdf
                                  atlk


lod :: FilePath -> IO ([Int],[MD.Geoset],[MD.TexUnit])
lod fname mpq = do let (s,n) = splitAt 3 $ reverse fname
                   let lname = assert (s=="2M.") (reverse $ "niks.00" ++ n)
                   archive <- BS.readFile lname
                   let view = decode archive :: MD.View
                   let bunchOf cnt offset g = getBunchOf cnt g (BS.drop offset bs) archive
                   let idlk = bunchOf (mv_nIndex_ view) (mv_ofsIndex_ view) getUShort
                   let tris = bunchOf (mv_nTris_ view)  (mv_ofsTris_ view)  getUShort
                   let gsdf = bunchOf (mv_nSub_ view)   (mv_ofsSub_ view)   (get :: MD.Geoset)
                   let txdf = bunchOf (mv_nTex_ view)   (mv_ofsTex_ view)   (get :: MD.TexUnit)
                   return $ (map (idlk!!) tris, gsdf, txdf)

renderpass :: [Texture]        -- all available textures
           -> [Geoset]         -- all available geosets
           -> [MD.RenderFlags] -- all available renderflags
           -> [MD.TransDef]
           -> [Int]         -- texture lookup
           -> [Int]         -- texture anim lookup
           -> [Int]         -- texture unit lookup
           -> TexUnit
           -> [RenderPass]
renderpass texdef geodef rflg trdf txlk talk tulk unit =
    let geoset = geodef !! tu_op_ unit
        flag   = rflg !! tu_flagsIndex_ unit
        tex       = texdef !! (txlk !! tu_textureid_ unit)
        blend     = rf_blend_   flag
        billboard = (rf_flags_ flag .&. 8) /= 0
        opacity   = trdf !! tu_transid_ unit
    in  RenderPass{ r_geoset_      = geoset
	          , r_tex_         = tex
                  , r_indexStart_  = mg_istart_ geoset
                  , r_indexCount_  = mg_icount_ geoset
                  , r_vertexStart_ = mg_vstart_ geoset
                  , r_vertexEnd_   = mg_vstart_ geoset + mg_vcount_ geoset
	          , r_useTex2_     = False
                  , r_useEnvMap_   = (tulk !! tu_texunit_ unit) == -1 && billboard && blend > 2
                  , r_cull_        = (rf_flags_ flag .&. 4) == 0 && blend == 0
                  , r_trans_       = blend>0 && opacity>0
                  , r_unlit_       = (rf_flags_ flag .&. 1) /= 0
                  , r_noZWrite_    = (rf_flags_ flag .&. 16) /= 0
                  , r_billboard_   = billboard
	          , r_p_           = z_ (mg_BoundingBox1_ geoset)
	          , r_texanim_     = -1                     -- fix-me, we should support texture animation
                  , r_color_       = tu_colorIndex_ unit    -- maybe should be a Color, rather than an index
                  , r_opacity_     = opacity
                  , r_blendmode_   = blend
	          , r_order_       = tu_shading_ unit
	          , r_swrap_       = (t_flags_ tex .&. 1) /= 0
                  , r_twrap_       = (t_flags_ tex .&. 2) /= 0
                  }    

    where z_ (Vector3 _ _ a) = a

instance Oder RenderPass where
    compare r1 r2 = case compare (r_order_ r1) (r_order_ r2) of
                      LT -> LT
                      GT -> GT
                      EQ -> case compare (r_blendmode_ r1) (r_blendmode_ r2) of
                              LT -> LT
                              GT -> GT
                              EQ -> compare (r_p_ r1) (r_p_ r2)

{--  blend: Value    Mapped to       Meaning
            0           0            Combiners_Opaque
            1           1            Combiners_Mod
            2           1            Combiners_Decal
            3           1            Combiners_Add
            4           1            Combiners_Mod2x
            5           4            Combiners_Fade
            6           4            Used in the Deeprun Tram subway glass, supposedly (src=dest_color, dest=src_color)
--}