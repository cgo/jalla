{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
module Math.Jalla.VectorSpace
( ) where

import Data.VectorSpace
import Math.Vector
import Math.InnerProduct

instance (InnerProduct o f) => InnerSpace o where
    Scalar o :: f
    a <.> b = innerProduct a b

instance AdditiveGroup (CVector (vec e) e) where
    zeroV = -- ... and here is where it breaks. I can not give a zero vector of unknown length.
