module M2Model where

import qualified ModelDef as MD
import Control.Exception

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

data RenderPass = RenderPass{ r_indexStart_, r_indexCount_
                            , r_vertexStart_, r_vertexEnd_ :: Int
	                    , r_tex_ :: Int
	                    , r_useTex2_, r_useEnvMap_, r_cull_, r_trans_
                            , r_unlit_, r_noZWrite_, r_billboard_ :: Bool
	                    , r_p_ :: Float
	                    , r_texanim_, r_color_, r_opacity_, r_blendmode_ :: Int
	                    , r_order_ :: Int
	                    , r_geoset_ :: Int
	                    , r_swrap_, r_twrap_ :: Bool
	                    , r_ocol_, r_ecol_ :: Vector4 Float
                            }

data Attachment = Attachment{ a_id_
	                    , a_bone_ :: Int
	                    , a_pos_  :: Vector3 Float
                            }

build :: FilePath -> IO M2Model
build fpath = do archive <- BS.readFile fpath
                 let def  = decode archive :: MD.Header
                 assert (nViews_ def > 0) (return ())
                 let bunchOf cnt offset g = runGet (sequence $ replicate cnt g) (BS.drop offset archive)
                 let gseq = bunchOf (nGlobalSequences_ def) (ofsGlobalSequences_ def) getWord32le
                 let vert = bunchOf (nVertices_ def) (ofsVertices_ def) (get :: Get MD.VertexDef)
                 let txdf = bunchOf (nTextures_ def) (ofsTextures_ def) (get :: Get MD.TextureDef)
                 let atdf = bunchOf (nAttachments_ def) (ofsAttachments_ def) (get :: Get MD.AttachmentDef)
                 let atlk = bunchOf (nAttachLookup_ def) (ofsAttachLookup_ def) getWord16le
                 let cldf = bunchOf (nColors_ def) (ofsColors_ def) (get :: Get MD.ColorDef)
                 let trdf = bunchOf (nTransparency_ def) (ofsTransparency_ def) (get :: MD.TransDef)
                 let trlk = bunchOf (nTransparencyLookup_ def) (ofsTransparencyLookup_ def) getWord16le
                 let rflg = bunchOf (nTexFlags_ def) (ofsTexFlags_ def) (get :: Get MD.RenderFlags)
                 let txlk = bunchOf (nTexLookup_ def) (ofsTexLookup_ def) getWord16le
                 let talk = bunchOf (nTexAnimLookup_ def) (ofsTexAnimLookup_ def) getWord16le
                 let tulk = bunchOf (nTexUnitLookup_ def) (ofsTexUnitLookup_ def) getWord16le
                 (indc, geoset, texunit) <- lod fpath
                 let rps  = map (renderpass txlk talk tulk) texunit
                 return $ M2Model fpath
                                  gseq
                                  vert
                                  indc 
                                  ??
                                  geoset
                                  rps
                                  ?? ?? 
                                  trlk 
                                  (map (\ad -> Attachment (ad_id_ ad) (ad_bone_ ad) (ad_pos_)) atdf)
                                  atlk


