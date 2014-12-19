{-# LANGUAGE BangPatterns #-}
module Data.WOW.M2Model( M2Model(..)
                       , RenderPass(..)
                       , Vertex
                       , Texture(..)
                       , Geoset
                       , Attachment
                       , Animation(..)
                       , openM2
                       , module Data.WOW.ModelDef) where

import Data.Bits((.&.))
import Data.Binary
import Data.List(sort)
-- import Data.Tensor
import Graphics.Rendering.OpenGL.GL.Tensor
import Text.Printf
import qualified Data.ByteString.Lazy as BS
import Control.Exception

import Data.WOW.Bone
import Data.WOW.Animated
import Data.WOW.FileSystem
import Data.WOW.Utils
import Data.WOW.ModelDef

data M2Model = M2Model{ m_global_sequence_ :: ![Int]
                      , m_vertices_        :: ![Vertex] -- vertices
                      , m_indices_         :: ![Int]    -- triagnles by index
                      , m_textures_        :: ![Texture]
                      , m_geoset_          :: ![Geoset]
                      , m_renderpass_      :: ![RenderPass]
                      , m_colors_          :: ![(Animated (Vector3 Float), Animated PackedFloat)]
                      , m_transparency_    :: ![Animated PackedFloat]
                      , m_trans_lookup_    :: ![Int]
                      , m_attachments_     :: ![Attachment]
                      , m_attach_lookup_   :: ![Int]
                      , m_bones_           :: ![Bone]
                      , m_keybone_lookup_  :: ![Int]
                      , m_animations_      :: ![Animation]
                      , m_anim_lookup_     :: ![Int]
                      , m_bounding_        :: PhysicsSettings
                      , m_bounding_volume_ :: ([Int],[Vector3 Float])
                      }

type Vertex = VertexDef

data Texture = Texture           { t_filename_ :: String, t_flags_ :: Int }
             | ReplacableTexture { t_type_     :: Int   , t_flags_ :: Int }

type Geoset = GeosetDef

data RenderPass = RenderPass{ r_geoset_ :: Int
	                    , r_tex_    :: Int
	                    , r_useTex2_, r_useEnvMap_, r_cull_, r_trans_
                            , r_unlit_, r_noZWrite_, r_billboard_ :: Bool
	                    , r_p_ :: Float
	                    , r_texanim_, r_color_, r_opacity_, r_blendmode_ :: Int
	                    , r_order_ :: Int
	                    , r_swrap_, r_twrap_ :: Bool
                            } deriving Show

type Attachment = AttachmentDef

data Animation  = Animation{ anim_Id_
                           , anim_subId_
                           , anim_length_     :: Int
                           , anim_move_speed_ :: Float
                           , anim_loop_  :: Bool
                           , anim_flags_ :: Int
                           }

openM2 :: FileSystem fs => fs -> String -> IO M2Model
openM2 fs fpath = do
  marchive <- findFile fs fpath
  let archive = case marchive of
                  Nothing -> error ("cannot find " ++ fpath) 
                  Just a  -> a
  let def  = decode archive :: Header
  assert (nViews_ def > 0) (return ())
             
  -- read a few definitions
  let bunchOf cnt offset g = getBunchOf cnt g (BS.drop (fromIntegral offset) archive)
      -- global sequence
      !gseq = bunchOf (nGlobalSequences_ def)    (ofsGlobalSequences_ def) getUInt
      -- vertex definition
      !vert = bunchOf (nVertices_ def)           (ofsVertices_ def)        (get :: Get VertexDef)
      -- texture definition
      !txdf = bunchOf (nTextures_ def)           (ofsTextures_ def)        (get :: Get TextureDef)
      -- attachment definition
      !atdf = bunchOf (nAttachments_ def)        (ofsAttachments_ def)     (get :: Get AttachmentDef)
      -- attachment lookups
      !atlk = bunchOf (nAttachLookup_ def)       (ofsAttachLookup_ def)    getUShort
      -- color definitions
      !cldf = bunchOf (nColors_ def)             (ofsColors_ def)          (get :: Get ColorDef)
      -- transparency definitions
      !trdf = bunchOf (nTransparency_ def)       (ofsTransparency_ def)    (get :: Get TransDef)
      -- transparency lookups
      !trlk = bunchOf (nTransparencyLookup_ def) (ofsTransparencyLookup_ def) getUShort
      -- render flags
      !rflg = bunchOf (nTexFlags_ def)           (ofsTexFlags_ def)        (get :: Get RenderFlags)
      -- texture lookups
      !txlk = bunchOf (nTexLookup_ def)          (ofsTexLookup_ def)       getUShort
      -- texture animation lookups
      !talk = bunchOf (nTexAnimLookup_ def)      (ofsTexAnimLookup_ def)   getUShort
      -- texture unit lookups
      !tulk = bunchOf (nTexUnitLookup_ def)      (ofsTexUnitLookup_ def)   getUShort
      -- bone definitions
      !bndf = bunchOf (nBones_ def)              (ofsBones_ def)           (get :: Get BoneDef)
      -- animation definitions
      !andf = bunchOf (nAnimations_ def)         (ofsAnimations_ def)      (get :: Get AnimationDef)
      -- key bone lookups
      !kblk = bunchOf (nKeyBoneLookup_ def)      (ofsKeyBoneLookup_ def)   getUShort
      -- animation lookups 
      !anlk = bunchOf (nAnimationLookup_ def)    (ofsAnimationLookup_ def) getUShort

      !bounding_indices = bunchOf (nBoundingTriangles_ def) (ofsBoundingTriangles_ def) getUShort
      !bounding_vertices  = bunchOf (nBoundingVertices_ def)  (ofsBoundingVertices_ def)  (get :: Get (Vector3 Float))
  
  -- textures
  let textures = assert (nTextures_ def < 32) $ 
                 map (\d -> case td_type_ d of
                                0 -> Texture (bunchOf (td_nameLen_ d - 1) (td_nameOfs_ d) (get :: Get Char)) (td_flags_ d)
                                _ -> ReplacableTexture (td_type_ d) (td_flags_ d)
                     ) txdf

  -- color table
  let color  = map (\(ColorDef clr opc) -> 
                        (newAnimated clr gseq archive [] ,newAnimated opc gseq archive [])) cldf
  -- transparency table
  let trans  = map (\tra -> newAnimated tra gseq archive []) trdf
      
  -- view
  (indices, geoset, texunit) <- lod fs fpath
  
  -- renderpasses
  let rps    = map (renderpass textures geoset rflg trlk txlk talk tulk) texunit

  -- bone
  (animations, bones) <- anim fs fpath archive gseq andf bndf
             
  -- create the model
  return $ M2Model{ m_global_sequence_ = gseq
                  , m_vertices_        = vert
                  , m_indices_         = indices
                  , m_textures_        = textures
                  , m_geoset_          = geoset
                  , m_renderpass_      = (sort rps)
                  , m_colors_          = color
                  , m_transparency_    = trans
                  , m_trans_lookup_    = trlk 
                  , m_attachments_     = atdf
                  , m_attach_lookup_   = atlk
                  , m_bones_           = bones
                  , m_keybone_lookup_  = kblk
                  , m_animations_      = animations
                  , m_anim_lookup_     = anlk 
                  , m_bounding_        = ps_ def
                  , m_bounding_volume_ = (bounding_indices, bounding_vertices)
                  }


lod :: FileSystem fs => fs -> FilePath -> IO ([Int],[GeosetDef],[TexUnitDef])
lod fs fname = do 
  let (s,n) = splitAt 3 $ reverse fname
      lname = assert (s=="2M." || s=="2m.") (reverse $ "niks.00" ++ n)
  Just archive <- findFile fs lname
  let bunchOf cnt offset g = getBunchOf cnt g (BS.drop (fromIntegral offset) archive)
      !view = decode archive :: ViewDef
      !idlk = bunchOf (mv_nIndex_ view) (mv_ofsIndex_ view) getUShort
      !tris = bunchOf (mv_nTris_ view)  (mv_ofsTris_ view)  getUShort
      !gsdf = bunchOf (mv_nSub_ view)   (mv_ofsSub_ view)   (get :: Get GeosetDef)
      !txdf = bunchOf (mv_nTex_ view)   (mv_ofsTex_ view)   (get :: Get TexUnitDef)
  return $ (map (idlk!!) tris, gsdf, txdf)

anim :: FileSystem fs => fs -> FilePath -> BS.ByteString -> [Int] -> [AnimationDef] -> [BoneDef] -> IO ([Animation], [Bone])
anim fs fname archive gseq ads bds 
    | x=="2M." || x=="2m." = do
  let !anims = map (\def -> Animation{ anim_Id_     = an_animId_ def
                                     , anim_subId_  = an_subAnimId_ def
                                     , anim_length_ = an_length_ def
                                     , anim_move_speed_ = an_moveSpeed_ def
                                     , anim_loop_   = an_loopType_ def > 0
                                     , anim_flags_  = an_flags_ def}
                   ) ads
  -- part of animations has a separate .anim file, which provides animated data
  files <- mapM (\a -> findFile fs $ (printf "%s%04d-%02d.anim" (reverse n) (anim_Id_ a) (anim_subId_ a))) anims
  let !bones = map (\def -> let !t = newAnimated (bn_tran_ def) gseq archive files
                                !r = newAnimated (bn_rota_ def) gseq archive files
                                !s = newAnimated (bn_scal_ def) gseq archive files
                            in Bone t r s (bn_pivot_ def) (bn_parent_ def) (bn_billboarded_ def)
                   ) bds
  return (anims, bones)

    | otherwise = error "unsupported file extension"
    
      where (x,n) = splitAt 3 $ reverse fname

renderpass :: [Texture]
           -> [Geoset]
           -> [RenderFlags]
           -> [Int]         -- transparency lookup
           -> [Int]         -- texture lookup
           -> [Int]         -- texture anim lookup
           -> [Int]         -- texture unit lookup
           -> TexUnitDef
           -> RenderPass
renderpass tx gs rflg trlk txlk _ tulk unit =
    let geoset    = tu_op_ unit
        flag      = rflg !! tu_flagsIndex_ unit
        tex       = (txlk !! tu_textureid_ unit)
        blend     = rf_blend_   flag
        billboard = (rf_flags_ flag .&. 8) /= 0
        opacity   = trlk !! tu_transid_ unit
    in  RenderPass{ r_geoset_      = geoset
	          , r_tex_         = tex
	          , r_useTex2_     = False
                  , r_useEnvMap_   = (tulk !! tu_texunit_ unit) == -1 && billboard && blend > 2
                  , r_cull_        = (rf_flags_ flag .&. 4) == 0 && blend == 0
                  , r_trans_       = blend>0 && opacity>0
                  , r_unlit_       = (rf_flags_ flag .&. 1) /= 0
                  , r_noZWrite_    = (rf_flags_ flag .&. 16) /= 0
                  , r_billboard_   = billboard
	          , r_p_           = z_ $ mg_BoundingBox1_ $ gs !! geoset
	          , r_texanim_     = -1                     -- fix-me, we should support texture animation
                  , r_color_       = tu_colorIndex_ unit    -- maybe should be a Color, rather than an index
                  , r_opacity_     = opacity
                  , r_blendmode_   = blend
	          , r_order_       = tu_shading_ unit
	          , r_swrap_       = (t_flags_ (tx !! tex) .&. 1) /= 0
                  , r_twrap_       = (t_flags_ (tx !! tex) .&. 2) /= 0
                  }    

    where z_ (Vector3 _ _ a) = a

instance Eq RenderPass where
    r1 == r2      = (r_order_ r1 == r_order_ r2) && 
                    (r_blendmode_ r1 == r_blendmode_ r2) &&
                    (r_p_ r1 == r_p_ r2)

instance Ord RenderPass where
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

instance Show Texture where
    show (Texture f _) = "Texture: " ++ f
    show (ReplacableTexture t _) = "Texture: " ++ show t

instance Show Animation where
    show a = printf "Animation: %d-%d. Length:%d. Speed:%.2f. Loop:%d. flags:%d"
                    (anim_Id_ a)
                    (anim_subId_ a)
                    (anim_length_ a)
                    (anim_move_speed_ a)
                    (fromEnum $ anim_loop_ a)
                    (anim_flags_ a)
