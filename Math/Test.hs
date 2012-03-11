{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Math.Test (Arbitrary(..),
                  prop_pseudoInverse) where

import BLAS.Foreign.BlasOps
import Math.Matrix
import Math.Vector
import Math.Types
import Test.QuickCheck
import System.Random


--instance (Random a, CMatrix mat a) => Random (mat a) where
--  randomR (lo,hi) g = createMatrix $ matrixMap

instance (Random e, RealFloat e) => Random (Complex e) where
  random g = (a' :+ b', g')
    where (a',g'') = random g
          (b',g')  = random g''
  randomR (a :+ b, c :+ d) g = (a' :+ b', g')
    where (a', g'') = randomR (a,c) g
          (b', g')  = randomR (b,d) g


class Bounds a where
  minB :: a
  maxB :: a


instance (RealFloat a) => Bounds a where
  minB = fromIntegral (minBound :: Int)
  maxB = fromIntegral (maxBound :: Int)
  
instance (Bounds a, RealFloat a) => Bounds (Complex a) where
  minB = (minB :+ minB)
  maxB = (maxB :+ maxB)


instance (Bounds e, BlasOps e, Random e) => Arbitrary (Matrix e) where
  arbitrary = do
    m <- choose (1,100)
    n <- choose (1,100)
    els <- vectorOf (m*n) (choose (minB, maxB))
    return $ listMatrix (m,n) els
    

-- FIXME: Implement this and others.
prop_pseudoInverse :: Matrix CDouble -> Bool
prop_pseudoInverse m = m == m

