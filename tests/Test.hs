{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

module Main (main,
             Arbitrary(..),
             prop_pseudoInverse,
             prop_frobNorm,
             prop_frobNorm2,
             prop_frobNorm3,
             prop_frobNorm4) where

import Jalla.BLAS.Foreign.BlasOps
import Jalla.Matrix
import Jalla.Vector
import Jalla.Types
import System.Random


import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import Data.List


main = defaultMain tests

tests = [
  testGroup "Matrix Norms" [
     testProperty "frob1" prop_frobNorm,
     testProperty "frob2" prop_frobNorm2,
     testProperty "frob3" prop_frobNorm3,
     testProperty "frob4" prop_frobNorm4
     ],
  testGroup "Multiplications" [
    testProperty "matrixMultDiag1" prop_matrixMultDiag1,
    testProperty "matrixMultDiag2" prop_matrixMultDiag2,
    testProperty "matrixMultDiag3" prop_matrixMultDiag3,
    testProperty "matrixMultDiag4" prop_matrixMultDiag4
     ],
  testGroup "Inverse" [
    testProperty "pseudoInverse" prop_pseudoInverse
    ]
  ]



--instance (Random a, CMatrix mat a) => Random (mat a) where
--  randomR (lo,hi) g = createMatrix $ matrixMap

instance (Random e, RealFloat e) => Random (Complex e) where
  random g = (a' :+ b', g')
    where (a',g'') = random g
          (b',g')  = random g''
  randomR (a :+ b, c :+ d) g = (a' :+ b', g')
    where (a', g'') = randomR (a,c) g
          (b', g')  = randomR (b,d) g''


class Bounds a where
  minB :: a
  maxB :: a


instance (RealFloat a) => Bounds a where
  minB = realToFrac (-1000) -- fromIntegral (minBound :: Int)
  maxB = realToFrac (1000)  -- fromIntegral (maxBound :: Int)
  
instance (Bounds a, RealFloat a) => Bounds (Complex a) where
  minB = (minB :+ minB)
  maxB = (maxB :+ maxB)


instance (Bounds e, BlasOps e, Random e) => Arbitrary (Matrix e) where
  arbitrary = do
    m   <- choose (1,100)
    n   <- choose (1,100)
    els <- vectorOf (m*n) (choose (minB, maxB))
    return $ listMatrix (m,n) els
    


prop_frobNorm :: Matrix CDouble -> Bool
prop_frobNorm m = (2 * abs (a - b) / (abs a + abs b)) <= 1e-8
  where a = frobNorm m 
        b = sqrt $ sum $ map (^2) $ matrixList RowMajor m
        
prop_frobNorm2 :: Matrix CFloat -> Bool
prop_frobNorm2 m = (2 * abs (a - b) / (abs a + abs b)) <= 1e-6
  where a = frobNorm m 
        b = realToFrac ((sqrt $ sum $ map ((^2) . realToFrac) $ matrixList RowMajor m) :: CDouble)
        
prop_frobNorm3 :: Matrix (Complex CFloat) -> Bool
prop_frobNorm3 m = 2 * realPart (abs (a - b)) / (realPart (abs a + abs b)) <= 1e-5
  where a = frobNorm m 
        b = sqrt $ sum $ map (^2) $ matrixList RowMajor m
        
prop_frobNorm4 :: Matrix (Complex CDouble) -> Bool
prop_frobNorm4 m = 2 * realPart (abs (a - b)) / (realPart (abs a + abs b)) <= 1e-8
  where a = frobNorm m 
        b = sqrt $ sum $ map (^2) $ matrixList RowMajor m


prop_matrixMultDiag :: (BlasOps a, RealFrac a) => Matrix a -> Bool
prop_matrixMultDiag mat = frobNorm (matrixMultDiag (mat,NoTrans) (map realToFrac [1..]) ##- mat ## dm) < 1e-8 &&
                          frobNorm (matrixMultDiag (mat,Trans) (map realToFrac [1..]) ##- (mat,Trans) ##! (dm',NoTrans)) < 1e-8
  where (m,n) = shape mat
        dm    = createMatrix (n,m) act
        dm'   = createMatrix (m,n) act
        act   = fill 0 >> (setDiag 0 $ map realToFrac [1..])

prop_matrixMultDiagC :: (BlasOpsComplex a, RealFloat a) => Matrix (Complex a) -> Bool
prop_matrixMultDiagC mat = realPart (frobNorm (a ##- mat ## dm)) < 1e-7 &&
                           realPart (frobNorm (b ##- (mat,Trans) ##! (dm',NoTrans))) < 1e-7
  where 
    a = matrixMultDiag (mat,NoTrans) (map realToFrac [1..])
    b = matrixMultDiag (mat,Trans) (map realToFrac [1..])
    (m,n) = shape mat
    dm    = createMatrix (n,m) act
    dm'   = createMatrix (m,n) act
    act   = fill 0 >> (setDiag 0 $ map realToFrac [1..])

prop_matrixMultDiag1 :: Matrix CFloat -> Bool
prop_matrixMultDiag1 = prop_matrixMultDiag
prop_matrixMultDiag2 :: Matrix CDouble -> Bool
prop_matrixMultDiag2 = prop_matrixMultDiag
prop_matrixMultDiag3 :: Matrix (Complex CFloat) -> Bool
prop_matrixMultDiag3 = prop_matrixMultDiagC
prop_matrixMultDiag4 :: Matrix (Complex CDouble) -> Bool
prop_matrixMultDiag4 = prop_matrixMultDiagC


prop_pseudoInverse :: Matrix CDouble -> Bool
prop_pseudoInverse mat = frobNorm a < 1e-6
  where
    (m,n) = shape mat
    mat_plus = pseudoInverse mat
    a | m < n     = idMatrix m ##- mat ## mat_plus
      | otherwise = idMatrix n ##- mat_plus ## mat
    
    
