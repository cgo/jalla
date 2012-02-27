{-# LANGUAGE PackageImports #-}

-- Just a quickly hacked program to try things out. Not a real test.

module Main where

import "jalla" Math.Matrix
import "jalla" Math.Vector
import Data.Maybe

main = 
    let
        m :: Matrix CFloat
        m = createMatrix (10,4) $ do
              fill 0
              setDiag 0 (repeat 1)
              setDiag 1 (repeat 2)
              setDiag (-1) (repeat (-2))
        m4 :: Matrix CFloat
        m4 = fromJust $ listMatrix (3,4) $ take (3*4) [1..]
        m2 :: Matrix CDouble
        m2 = matrixMap ((+2) . realToFrac ) m
        m3 :: Matrix CFloat
        m3 = matrixBinMap (\a b -> realToFrac a + realToFrac b) m m
        m5 :: Matrix CDouble
        m5 = matrixBinMap (\a b -> realToFrac (a + b)) m4 m4
        m5svd = svd m5 (SVDU SVDFull, SVDVT SVDFull) :: SVD Matrix CDouble Vector CDouble
    in
     do
      prettyPrintMatrixIO m
      print "Mapped: " 
      prettyPrintMatrixIO m2
      print "m + m2: "
      prettyPrintMatrixIO m3
      print "m4: " 
      prettyPrintMatrixIO m4 
      print "m4 + m4: " 
      prettyPrintMatrixIO (m4 ##+ m4) 
      print "m4 - m4: " 
      prettyPrintMatrixIO (m4 ##- m4) 
      print "m5:"  
      prettyPrintMatrixIO m5 
      print "svd, singular values of m5:"  
      print (svdS m5svd) 
      print "svd, U:" 
      prettyPrintMatrixIO (fromJust $ svdU m5svd) 
      print "svd, VT:" 
      prettyPrintMatrixIO (fromJust $ svdVT m5svd) 
      let s = (createMatrix (shape m5) $ fill 0 >> setDiag 0 (vectorList (svdS m5svd))) :: Matrix CDouble
          u = fromJust $ svdU m5svd
          vt = fromJust $ svdVT m5svd
      print "U * S * VT = "
      prettyPrintMatrixIO $ u ## s ## vt