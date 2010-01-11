{-# OPTIONS -XTypeFamilies -XGeneralizedNewtypeDeriving #-}
module Data.WOW.Quaternion(Quaternion(..), slerp, rotate, identityQ) where

import Data.Cross
import Data.VectorSpace
import Data.Array

import Data.WOW.Matrix

newtype Quaternion = Quaternion (Float,Float,Float,Float) deriving (Show,InnerSpace,AdditiveGroup)

instance VectorSpace Quaternion where
    type Scalar Quaternion = Float
    v *^ (Quaternion q) = Quaternion (v *^ q)

slerp :: Quaternion -> Quaternion -> Float -> Quaternion
slerp q1 q2 u = normalized $ (a *^ q1') ^+^ (b *^ q2')
    where
      t   = acos $ q1' <.> q2'
      st  = sin t
      a   = (sin $ (1 - u) * t) / st
      b   = (sin $ u * t) / st
      q1' = normalized q1
      q2' = normalized q2


rotate :: Quaternion -> Matrix
rotate (Quaternion (i,j,k,r))
    = let r2 = r^2; i2 = i^2; j2 = j^2; k2 = k^2
          ri = 2*r*i; rj = 2*r*j; rk = 2*r*k
          jk = 2*j*k; ki = 2*k*i; ij = 2*i*j
      in  r2 `seq` i2 `seq` j2 `seq` k2 `seq`
          ri `seq`rj `seq` rk `seq` jk `seq` ki `seq` ij `seq`
          listArray ((0,0),(3,3)) [ r2+i2-j2-k2, ij-rk       , ki+rj       , 0
                                  , ij+rk      , r2-i2+j2-k2 , jk-ri       , 0 
                                  , ki-rj      , jk+ri       , r2-i2-j2+k2 , 0
                                  , 0          , 0           , 0           , 1 ]
identityQ = Quaternion (0,0,0,1)

{--
type Vec = (Float,Float,Float)
rot :: Vec -> Vec -> Quanternion
rot s t = let cst = cross3 s t
              dst = s <.> t
              a   = sqrt (2 + 2 * dst)
              (i,j,k) = cst ^/ a
          in  (i, j, k, a/2) 

mat :: Vec -> Vec -> Array (Int,Int) Float
mat s t = let v@(vx,vy,vz) = cross3 s t
              e = s <.> t
              h = (1 - e) / (v <.> v)
          in  listArray ((0,0),(3,3)) [ e + h*(vx^2), h*vx*vy-vz , h*vx*vz+vy, 0
                                      , h*vx*vy+vz  , e+h*(vy^2) , h*vy*vz-vx, 0
                                      , h*vx*vz-vy  , h*vy*vz+vx , e+h*(vz^2), 0
                                      , 0           , 0          , 0         , 1]
--}