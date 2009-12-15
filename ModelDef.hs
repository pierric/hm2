{-# OPTIONS -XFlexibleInstances #-}
module ModelDef where

import Data.Binary
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Word
import Data.Int
import Control.Monad(ap)
import Data.Tensor
import Text.Printf
import Utils

data Header = Header{ id_ :: String
                    , version_ :: [Int]
                    , nameLength_            , nameOfs_
                    , globalModelFlags_ 
                    , nGlobalSequences_      , ofsGlobalSequences_
                    , nAnimations_           , ofsAnimations_
                    , nAnimationLookup_      , ofsAnimationLookup_
                    , nBones_                , ofsBones_
                    , nKeyBoneLookup_        , ofsKeyBoneLookup_
                    , nVertices_             , ofsVertices_
                    , nViews_
                    , nColors_               , ofsColors_
                    , nTextures_             , ofsTextures_
                    , nTransparency_         , ofsTransparency_
                    , nTexAnims_             , ofsTexAnims_
                    , nTexReplace_           , ofsTexReplace_
                    , nTexFlags_             , ofsTexFlags_
                    , nBoneLookup_           , ofsBoneLookup_
                    , nTexLookup_            , ofsTexLookup_
                    , nTexUnitLookup_        , ofsTexUnitLookup_
                    , nTransparencyLookup_   , ofsTransparencyLookup_
                    , nTexAnimLookup_        , ofsTexAnimLookup_  :: Int
                    , ps_ :: PhysicsSettings
                    , nBoundingTriangles_    , ofsBoundingTriangles_
                    , nBoundingVertices_     , ofsBoundingVertices_
                    , nBoundingNormals_      , ofsBoundingNormals_
                    , nAttachments_          , ofsAttachments_
                    , nAttachLookup_         , ofsAttachLookup_
                    , nEvents_               , ofsEvents_
                    , nLights_               , ofsLights_
                    , nCameras_              , ofsCameras_
                    , nCameraLookup_         , ofsCameraLookup_
                    , nRibbonEmitters_       , ofsRibbonEmitters_
                    , nParticleEmitters_     , ofsParticleEmitters_ :: Int
                    }

data PhysicsSettings = PhysicsSettings{ vertexBox1_, vertexBox2_ :: Vector3 Float
                                      , vertexRadius_ :: Float
                                      , boundingBox1_, boundingBox2_ :: Vector3 Float
                                      , boundingRadius_ :: Float 
                                      }

data TextureDef = TextureDef{ td_type_, td_flags_, td_nameLen_, td_nameOfs_ :: Int }

data TexUnit    = TexUnit{ tu_flags_
                         , tu_shading_
	                 , tu_op_
	                 , tu_op2_ -- :: Word16
	                 , tu_colorIndex_ -- :: Int16
	                 , tu_flagsIndex_
	                 , tu_texunit_
	                 , tu_mode_
	                 , tu_textureid_
	                 , tu_texunit2
	                 , tu_transid_
	                 , tu_texanimid_ :: Int -- :: Word16
                         }

data Geoset = Geoset{ mg_id_
	            , mg_vstart_
	            , mg_vcount_
	            , mg_istart_
	            , mg_icount_
	            , mg_nBones_
	            , mg_StartBones_
	            , mg_d5_
	            , mg_rootBone_ :: Int
	            , mg_BoundingBox1_, mg_BoundingBox2_ :: Vector3 Float
	            , mg_radius_ :: Float
                    }

data View = View { mv_id_ :: String
                 , mv_nIndex_
                 , mv_ofsIndex_
                 , mv_nTris_
	         , mv_ofsTris_
                 , mv_nProps_
	         , mv_ofsProps_
                 , mv_nSub_
	         , mv_ofsSub_
                 , mv_nTex_
	         , mv_ofsTex_
	         , mv_lod_ :: Int
                 }
data AnimationBlock = AnimationBlock{ ab_type_
	                            , ab_seq_
	                            , ab_nTimes_
	                            , ab_ofsTimes_
	                            , ab_nKeys_
	                            , ab_ofsKeys_ :: Int
                                    }
data AttachmentDef = AttachmentDef{ ad_id_
	                          , ad_bone_ :: Int
	                          , ad_pos_  :: Vector3 Float
                                  }

type TransDef = AnimationBlock

data VertexDef = VertexDef{ vd_pos_ :: Vector3 Float
	                  , vd_weights_ :: [Int]
	                  , vd_bones_ :: [Int]
	                  , vd_normal_ :: Vector3 Float
	                  , vd_texcoords_ :: Vector2 Float
	                  , vd_unk1_, vd_unk2_ :: Int
                          }

data ColorDef = ColorDef{  cf_color_, cf_opacity_ :: AnimationBlock }

data RenderFlags = RenderFlags{ rf_flags_, rf_blend_ :: Int }

instance Binary Header where
    get   = return Header 
                       `ap` (sequence [cc,cc,cc,cc])  -- id_
                       `ap` (sequence [uc,uc,uc,uc])  -- version_
                       `ap` ui `ap` ui  -- nameLength_            , nameOfs_
                       `ap` ui          -- globalModelFlags_
                       `ap` ui `ap` ui  -- nGlobalSequences_      , ofsGlobalSequences_
                       `ap` ui `ap` ui  -- nAnimations_           , ofsAnimations_
                       `ap` ui `ap` ui  -- nAnimationLookup2_     , ofsAnimationLookup_
                       `ap` ui `ap` ui  -- nBones_                , ofsBones_
                       `ap` ui `ap` ui  -- nKeyBoneLookup_        , ofsKeyBoneLookup_
                       `ap` ui `ap` ui  -- nVertices_             , ofsVertices_
                       `ap` ui          -- nViews_
                       `ap` ui `ap` ui  -- nColors_               , ofsColors_
                       `ap` ui `ap` ui  -- nTextures_             , ofsTextures_
                       `ap` ui `ap` ui  -- nTransparency_         , ofsTransparency_
                       `ap` ui `ap` ui  -- nTexAnims_             , ofsTexAnims_
                       `ap` ui `ap` ui  -- nTexReplace_           , ofsTexReplace_
                       `ap` ui `ap` ui  -- nTexFlags_             , ofsTexFlags_
                       `ap` ui `ap` ui  -- nBoneLookup_           , ofsBoneLookup_
                       `ap` ui `ap` ui  -- nTexLookup_            , ofsTexLookup_
                       `ap` ui `ap` ui  -- nTexUnitLookup_        , ofsTexUnitLookup_
                       `ap` ui `ap` ui  -- nTransparencyLookup_   , ofsTransparencyLookup_
                       `ap` ui `ap` ui  -- nTexAnimLookup_        , ofsTexAnimLookup_  :: Int
                       `ap` (get :: Get PhysicsSettings) --  ps_
                       `ap` ui `ap` ui  -- nBoundingTriangles_    , ofsBoundingTriangles_
                       `ap` ui `ap` ui  -- nBoundingVertices_     , ofsBoundingVertices_
                       `ap` ui `ap` ui  -- nBoundingNormals_      , ofsBoundingNormals_
                       `ap` ui `ap` ui  -- nAttachments_          , ofsAttachments_
                       `ap` ui `ap` ui  -- nAttachLookup_         , ofsAttachLookup_
                       `ap` ui `ap` ui  -- nEvents_               , ofsEvents_
                       `ap` ui `ap` ui  -- nLights_               , ofsLights_
                       `ap` ui `ap` ui  -- nCameras_              , ofsCameras_
                       `ap` ui `ap` ui  -- nCameraLookup_         , ofsCameraLookup_
                       `ap` ui `ap` ui  -- nRibbonEmitters_       , ofsRibbonEmitters_
                       `ap` ui `ap` ui  -- nParticleEmitters_     , ofsParticleEmitters_ :: Int

    put _ = error "cannot save Header"

instance Binary PhysicsSettings where
    get   = return PhysicsSettings `ap` get `ap` get
                                   `ap` getFloat32le
                                   `ap` get `ap` get
                                   `ap` getFloat32le
    put _ = error "cannot save PhysicsSettings"

instance Binary TextureDef where
    get   = return TextureDef `ap` ui `ap` ui `ap` ui `ap` ui
    put _ = error "cannot save TextureDef"

instance Binary TexUnit where
    get   = return TexUnit `ap` us `ap` us `ap` us `ap` us
                           `ap` us `ap` us `ap` us `ap` us
                           `ap` us `ap` us `ap` us `ap` us
    put _ = error "cannot save TexUnit"

instance Binary Geoset where
    get   = return Geoset `ap` ui 
                          `ap` us `ap` us `ap` us `ap` us
                          `ap` us `ap` us `ap` us `ap` us
                          `ap` get `ap` get
                          `ap` getFloat32le
    put _ = error "cannot save Geoset"

instance Binary View where
    get   = return View `ap` (sequence [cc,cc,cc,cc])
                        `ap` ui `ap` ui `ap` ui `ap` ui
                        `ap` ui `ap` ui `ap` ui `ap` ui
                        `ap` ui `ap` ui `ap` ui
    put _ = error "cannot save View"

instance Binary AnimationBlock where
    get   = return AnimationBlock `ap` us `ap` us
                                  `ap` ui `ap` ui `ap` ui `ap` ui 
    put _ = error "cannot save AnimationBlock" where                    

instance Binary AttachmentDef where
    get   = do a <- ui
               b <- ui
               p <- get
               get :: Get AnimationBlock
               return $ AttachmentDef a b p
    put _ = error "cannot save AttachmentDef"

instance Binary VertexDef where
    get   = return VertexDef `ap` get  -- pos
                             `ap` sequence [uc,uc,uc,uc] -- weight
	                     `ap` sequence [uc,uc,uc,uc] -- bone
	                     `ap` get -- normal
	                     `ap` get -- texcoords
	                     `ap` ui `ap` ui
    put _ = error "cannot save VertexDef"

instance Binary ColorDef where
    get   = return ColorDef `ap` get `ap` get
    put _ = error "cannot save ColorDef"

instance Binary RenderFlags where
    get   = return RenderFlags `ap` us `ap` us
    put _ = error "cannot save RenderFlags"

instance Show Header where
    show h = unlines [ printf "id:%s & version:%d %d %d %d" (id_ h) (v!!0) (v!!1) (v!!2) (v!!3)
                     , printf "name:%d,0x%x" (nameLength_ h) (nameOfs_ h)
                     , printf "GlobalModelFlags:%d" (globalModelFlags_ h)
                     , printf "GlobalSequences:%d,0x%x" (nGlobalSequences_ h) (ofsGlobalSequences_ h)
                     , printf "Animations:%d,0x%x" (nAnimations_ h) (ofsAnimations_ h)
                     , printf "AnimationLookup:%d,0x%x" (nAnimationLookup_ h) (ofsAnimationLookup_ h)
                     , printf "Bones:%d,0x%x" (nBones_ h) (ofsBones_ h)
                     , printf "KeyBoneLookup:%d,0x%x" (nKeyBoneLookup_ h) (ofsKeyBoneLookup_ h)
                     , printf "Vertices:%d,0x%x" (nVertices_ h) (ofsVertices_ h)
                     , printf "Views:%d" (nViews_ h)
                     , printf "Colors:%d,0x%x" (nColors_ h) (ofsColors_ h)
                     , printf "Textures:%d,0x%x" (nTextures_ h) (ofsTextures_ h)
                     , printf "Transparency:%d,0x%x" (nTransparency_ h) (ofsTransparency_ h)
                     , printf "TexAnims:%d,0x%x" (nTexAnims_ h) (ofsTexAnims_ h)
                     , printf "TexReplace:%d,0x%x" (nTexReplace_ h) (ofsTexReplace_ h)
                     , printf "TexFlags:%d,0x%x" (nTexFlags_ h) (ofsTexFlags_ h)
                     , printf "BoneLookup:%d,0x%x" (nBoneLookup_ h) (ofsBoneLookup_ h)
                     , printf "TexLookup:%d,0x%x" (nTexLookup_ h) (ofsTexLookup_ h)
                     , printf "TexUnitLookup:%d,0x%x" (nTexUnitLookup_ h) (ofsTexUnitLookup_ h)
                     , printf "TransparencyLookup:%d,0x%x" (nTransparencyLookup_ h) (ofsTransparencyLookup_ h)
                     , printf "TexAnimLookup:%d,0x%x" (nTexAnimLookup_ h) (ofsTexAnimLookup_ h)
                     , show (ps_ h)
                     , printf "BoundingTriangles:%d,0x%x" (nBoundingTriangles_ h) (ofsBoundingTriangles_ h)
                     , printf "BoundingVertices:%d,0x%x" (nBoundingVertices_ h) (ofsBoundingVertices_ h)
                     , printf "BoundingNormals:%d,0x%x" (nBoundingNormals_ h) (ofsBoundingNormals_ h)
                     , printf "Attachments:%d,0x%x" (nAttachments_ h) (ofsAttachments_ h)
                     , printf "AttachLookup:%d,0x%x" (nAttachLookup_ h) (ofsAttachLookup_ h)
                     , printf "Events:%d,0x%x" (nEvents_ h) (ofsEvents_ h)
                     , printf "Lights:%d,0x%x" (nLights_ h) (ofsLights_ h)
                     , printf "Cameras:%d,0x%x" (nCameras_ h) (ofsCameras_ h)
                     , printf "CameraLookup:%d,0x%x" (nCameraLookup_ h) (ofsCameraLookup_ h)
                     , printf "RibbonEmitters:%d,0x%x" (nRibbonEmitters_ h) (ofsRibbonEmitters_ h)
                     , printf "ParticleEmitters:%d,0x%x" (nParticleEmitters_ h) (ofsParticleEmitters_ h) 
                     ]
        where v = version_ h

instance Show PhysicsSettings where
    show p = "PhysicsSettings:" ++ "vertexBox " ++ showVecD (vertexBox1_ p) ++ " " ++ showVecD (vertexBox2_ p) ++
             "\n                " ++ printf "vertexRadius %.2f" (vertexRadius_ p) ++
             "\n                " ++ "boundingBox " ++ showVecD (boundingBox1_ p) ++ " " ++ showVecD (boundingBox2_ p) ++ 
             "\n                " ++ printf "boundingRadius %.2f" (boundingRadius_ p)

instance Show View where
    show v = unlines [ printf "id:%s" (mv_id_ v)
                     , printf "nIndex:%d" (mv_nIndex_ v)
                     , printf "ofsIndex:0x%x" (mv_ofsIndex_ v)
                     , printf "nTris:%d" (mv_nTris_ v)
	             , printf "ofsTris:0x%x" (mv_ofsTris_ v)
                     , printf "nProps:%d" (mv_nProps_ v)
	             , printf "ofsProps:0x%x" (mv_ofsProps_ v)
                     , printf "nSub:%d" (mv_nSub_ v)
	             , printf "ofsSub:0x%x" (mv_ofsSub_ v)
                     , printf "nTex:%d" (mv_nTex_ v)
	             , printf "ofsTex: 0x%x" (mv_ofsTex_ v)
	             , printf "lod:%d" (mv_lod_ v) ]

showVecD (Vector3 a b c) = printf "(%.2f,%.2f,%.2f)" a b c


cc :: Get Char
cc = get
uc,ui,us :: Get Int
uc = fmap fromIntegral getWord8
us = fmap (fromIntegral . (fromIntegral :: Word16 -> Int16)) getWord16le
ui = fmap fromIntegral getWord32le