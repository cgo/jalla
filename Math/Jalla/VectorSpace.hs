{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
module Math.Jalla.VectorSpace
( ) where

import Data.VectorSpace
import Math.Vector
import Math.InnerProduct

instance (InnerProduct o f) => InnerSpace o where
    Scalar o :: f
    a <.> b = innerProduct a b

instance AdditiveGroup (CVector (vec e) e)