{-# LANGUAGE PackageImports #-}

-- Just a quickly hacked program to try things out. Not a real test.

module Main where

import "jalla" Math.Matrix
import "jalla" Math.Vector
import Data.Maybe
import System.Random

main = 
    let
        m :: Matrix CFloat
        m = createMatrix (10,4) $ do
              fill 0
              setDiag 0 (repeat 1)
              setDiag 1 (repeat 2)
              setDiag (-1) (repeat (-2))
        m4 :: Matrix CFloat
        m4 = listMatrix (3,4) $ take (3*4) [1..]
        m2 :: Matrix CDouble
        m2 = matrixMap ((+2) . realToFrac ) m
        m3 :: Matrix CFloat
        m3 = matrixBinMap (\a b -> realToFrac a + realToFrac b) m m
        m5 :: Matrix CDouble
        m5 = matrixBinMap (\a b -> realToFrac (a + b)) m4 m4
        m5svd = svd m5 (SVDU SVDFull, SVDVT SVDFull)
        v1 :: Vector CFloat
        v1 = listVector [1,2,3]
        v2 = v1 |.* 2 
        randMatrix :: Int -> Int -> IO (Matrix CDouble)
        randMatrix m n = getStdGen >>= \g -> return (listMatrix (m,n) (repeat 1))        
        -- randMatrix m n = return $ createMatrix (m,n) $ fill 1
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
      let s = (createMatrix (shape m5) $ fill 0 >> setDiag 0 (svdS m5svd)) :: Matrix CDouble
          u = fromJust $ svdU m5svd
          vt = fromJust $ svdVT m5svd
      print "U * S * VT = "
      prettyPrintMatrixIO $ u ## s ## vt
      print v1
      print v2
      
      print "pseudo inverse m5' of m5:"
      -- prettyPrintMatrixIO $ pseudoInverse m5
      print "m5 * m5':"
      prettyPrintMatrixIO $ m5 ## (pseudoInverse m5)
      print "m5' * m5:"
      prettyPrintMatrixIO $ (pseudoInverse m5) ## m5
      
      m <- randMatrix 3 4
      print "Random matrix m:"
      prettyPrintMatrixIO m
      print "pseudo inverse m' of m:"
      prettyPrintMatrixIO $ pseudoInverse m
      print "m * m':"
      prettyPrintMatrixIO $ m ## (pseudoInverse m)
      print "m' * m:"
      prettyPrintMatrixIO $ (pseudoInverse m) ## m
      
      print "Row 0 of m:"
      print $ (row m 0 :: Vector CDouble)
      
      print "Rows of m:"
      mapM_ (putStrLn . show) $ (rows m :: [Vector CDouble])
      print "Columns of m:"
      mapM_ (putStrLn . show) $ (columns m :: [Vector CDouble])
      
      g <- getStdGen
      bigrand <- randMatrix 1000 100000
      let d = matrixMultDiag bigrand (randoms g) -- [1,2,3,4,5]
      print "Columns of m5 * something diagonal:"
      print (colCount d)
      print (rowCount d)
--      mapM_ print d
--      prettyPrintMatrixIO bigrand