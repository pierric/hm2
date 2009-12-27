module Data.WOW.Matrix where

import Data.Array
import Data.Tensor

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

identity4 = listArray ((0,0),(3,3)) [1,0,0,0
                                    ,0,1,0,0
                                    ,0,0,1,0
                                    ,0,0,0,1]

translation :: Vector3 Float -> Matrix
translation (Vector3 x y z) = identity4 // [((0,3),x),((1,3),y),((2,3),z)]

scale :: Vector3 Float -> Matrix
scale (Vector3 x y z) = identity4 // [((0,0),x),((1,1),y),((2,2),z)]

