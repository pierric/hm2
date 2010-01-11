module Data.WOW.Bone(Bone(..), transform)  where

import Control.Arrow((&&&))
import Data.Tensor
import Data.VectorSpace(negateV)

import Data.WOW.Animated
import Data.WOW.Quaternion
import Data.WOW.Matrix

data Bone  = Bone{ bone_tran_   :: Animated (Vector3 Float)
                 , bone_rota_   :: Animated PackedQuaternion
                 , bone_scal_   :: Animated (Vector3 Float)
                 , bone_pivot_  :: Vector3 Float
                 , bone_parent_ :: Int 
                 , bone_billboarded_ :: Bool
                 }
           
transform :: Matrix -> Int -> Int -> [Bone] -> [Matrix]
transform view anim time bones = map snd $ update [(-1, identity4)] (zip bones (repeat identity4)) False
    where
      update :: [(Int,Matrix)]    -- parents and their matrics
             -> [(Bone,Matrix)]   -- bones and their matrics
             -> Bool              -- is iteration done?
             -> [(Bone,Matrix)]
      update pm bn False = 
          let -- update each bone whose parent id is in pm
              lst = map upd bn
              -- the updated bone and transform pairs
              nbn = map fst lst
              -- those updated bone id and transform pairs
              npm = map ( fst &&& (snd . fst . snd)) $ filter (snd . snd) $ zip [0..] lst
              upd = \(b,m) -> case lookup (bone_parent_ b) pm of
                                Just pmat -> let p = bone_pivot_ b
                                                 t = at (bone_tran_ b) anim time (Vector3 0 0 0)
                                                 r = at (bone_rota_ b) anim time identityQ
                                                 s = at (bone_scal_ b) anim time (Vector3 1 1 1)
                                             in  ((b, pmat `mult` translation p
                                                           `mult` translation t 
                                                           `mult` rotate r
                                                           `mult` scale s
                                                           `mult` translation (negateV p) )
                                                 ,True)
                                Nothing   -> ((b,m), False)
          in  update npm nbn (null npm)
      update _ bn True = bn