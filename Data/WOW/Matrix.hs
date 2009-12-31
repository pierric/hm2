module Data.WOW.Matrix where

import Data.Array
import Data.Tensor
import Control.Exception

type Matrix = Array (Int,Int) Float

mult :: Matrix -> Matrix -> Matrix 
mult x y = accumArray (+) 0 rbd [ ((i,j),x!(i,k) * y!(k,j))
                                | i <- range (li,ui)
                                , j <- range (lj',uj')
                                , k <- range (lj,uj) ]
    where ((li ,lj ),(ui ,uj )) = bounds x
          ((li',lj'),(ui',uj')) = bounds y
          rbd | (lj,uj) == (li',ui') = ((li,lj'),(ui,uj'))
              | otherwise            = error "mult: incompatible bounds"

{--
multVec4 :: Matrix -> Vector4 Float -> Vector4 Float
multVec4 mat (Vector4 x y z 1) = assert (bounds mat == ((0,0),(3,3))) $ -- must be 4x4 matrix
                                 let v = [x,y,z,1]
                                     r = accumArray (+) 0 (0,3) [ (i, mat!(i,k) * (v!!k)) | i<-[0..3], k <- [0..3]]
                                 in  v `seq` r `seq` Vector4 (r!0) (r!1) (r!2) 1
--}

multVec3 :: Matrix -> Vector3 Float -> Vector3 Float
multVec3 mat (Vector3 x y z) = assert (bounds mat == ((0,0),(3,3))) $ -- must be 4x4 matrix
                               let v = [x,y,z,1]
                                   r = accumArray (+) 0 (0,3) [ (i, mat!(i,k) * (v!!k)) | i<-[0..3], k <- [0..3]]
                               in  v `seq` r `seq` Vector3 (r!0) (r!1) (r!2)

identity4 = listArray ((0,0),(3,3)) [1,0,0,0
                                    ,0,1,0,0
                                    ,0,0,1,0
                                    ,0,0,0,1]

translation :: Vector3 Float -> Matrix
translation (Vector3 x y z) = identity4 // [((0,3),x),((1,3),y),((2,3),z)]

scale :: Vector3 Float -> Matrix
scale (Vector3 x y z) = identity4 // [((0,0),x),((1,1),y),((2,2),z)]

fromList :: RealFloat a => [a] -> Matrix
fromList arr = assert (length arr == 16) $ listArray ((0,0),(3,3)) (map realToFrac arr)