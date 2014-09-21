module Data.WOW.Matrix(Matrix,mult, multVec3, identity4, translation, scale, inverse, fromList) where

import Graphics.Rendering.OpenGL.GL.Tensor
import qualified Numeric.LinearAlgebra as L
import Control.Exception

type Matrix = L.Matrix Double

mult :: Matrix -> Matrix -> Matrix 
mult = (L.<>)

multVec3 :: Matrix -> Vector3 Float -> Vector3 Float
multVec3 mat (Vector3 x y z) = assert (L.rows mat == 4 && L.cols mat == 4) $ -- must be 4x4 matrix
                               let [x',y',z',1] = L.toList $ head $ L.toColumns $ mat L.<> (4 L.>< 1) [realToFrac x, realToFrac y, realToFrac z, 1]
                               in  Vector3 (realToFrac x') (realToFrac y') (realToFrac z')

inverse :: Matrix -> Matrix
inverse = L.inv

identity4 :: Matrix
identity4 = L.diag $ L.fromList [1,1,1,1]

translation :: Vector3 Float -> Matrix
translation (Vector3 x y z) = (4 L.>< 4) [1,0,0,realToFrac x
                                         ,0,1,0,realToFrac y
                                         ,0,0,1,realToFrac z
                                         ,0,0,0,           1]

scale :: Vector3 Float -> Matrix
scale (Vector3 x y z) = (4 L.>< 4) [realToFrac x,0,0,0
                                   ,0,realToFrac y,0,0
                                   ,0,0,realToFrac z,0
                                   ,0,0,0,           1]

fromList :: RealFloat a => [a] -> Matrix
fromList arr = assert (length arr == 16) $ (4 L.>< 4) (map realToFrac arr)
