{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

module Numeric.Jalla.Test where

import Numeric.Jalla.Matrix
import Numeric.Jalla.Vector
import Numeric.Jalla.Foreign.BlasOps
import Numeric.Jalla.Types

import System.Random
import Test.QuickCheck

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
    

instance (Bounds e, BlasOps e, Random e) => Arbitrary (Vector e) where
  arbitrary = do
    m   <- choose (1,100)
    els <- vectorOf m (choose (minB, maxB))
    return $ listVector els

