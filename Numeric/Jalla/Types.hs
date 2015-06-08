{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeFamilies #-}
-----------------------------------------------------------------------------
--
-- Module      :  Math.Types
-- Copyright   :  2011 by Christian Gosch
-- License     :  BSD3
--
-- Maintainer  : Christian Gosch <werbung@goschs.de>
-- Stability   : Experimental
-- Portability : GHC only
--
-- | Contains some types used by Jalla, including some BLAS/LAPACK related ones.
-----------------------------------------------------------------------------

module Numeric.Jalla.Types (
  -- * Classes
  Field1(..),
  -- ** BLAS And LAPACK 
  BLASEnum(..),
  LAPACKEEnum(..),
  -- * Indexing
  Index,
  Shape,
  IndexPair,
  rowCountTrans,
  colCountTrans,
  shapeTrans,
  diagIndices,
  -- * Information About Matrices And Storage
  Order(..),
  Transpose(..),
  UpLo(..),
  module Data.Complex,
) where

import Data.Complex
import Data.Orphans ()
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign
import qualified Data.Tuple as T (swap)


type Index = Int
type Shape = (Index,Index)
type IndexPair = (Index,Index)


{-| Row count of a matrix with given transposedness and shape. -}
rowCountTrans :: Transpose -> Shape -> Index
rowCountTrans t (r,c) | t == Trans = c
                      | otherwise = r

{-| Column count of a matrix with given transposedness and shape. -}
colCountTrans :: Transpose -> Shape -> Index
colCountTrans t (r,c) | t == Trans = r
                      | otherwise = c

{-| Shape of a matrix with given transposedness and shape. -}
shapeTrans :: Transpose -> Shape -> Shape
shapeTrans t s | t == Trans = T.swap s
               | otherwise = s


{-| Generate indices of a diagonal in a matrix of given shape. -}
diagIndices :: Shape     -- ^ The shape of the matrix (rows,columns)
              -> Index   -- ^ The index of the diagonal -- 0: main diagonal; < 0: lower diagonals; >0: upper diagonals
              -> [IndexPair] -- ^ Index list. Empty if there is no such diagonal.
diagIndices (r,c) d
  | d >= 0 && d < c    = diagIndices' (0, d, min (c - d) r)
  | d < 0 && d > (-r) = diagIndices' (-d, 0, min (r + d) c)
  | otherwise        = []
    where
      diagIndices' :: (Index,Index,Index) -> [(Index,Index)]
      diagIndices' (rstart,cstart,n) = [(rstart + i, cstart + i) | i <- [0..(max 0 (n-1))]]


data Order = RowMajor | ColumnMajor deriving (Eq, Show)
-- type Order = CblasOrder
data Transpose = Trans | NoTrans deriving (Eq, Show)
data UpLo = Up | Lo deriving (Eq, Show)

class BLASEnum e be where
  toBlas :: e -> be
  fromBlas :: be -> e

class LAPACKEEnum e le where
  toLapacke :: e -> le
  fromLapacke :: le -> e

f :: Complex a -> a
f _ = undefined

{-| Defines a scalar type for each field type. Those are 'Complex' 'CFloat'
    and 'CFloat', as well as 'Complex' 'CDouble' and 'CDouble'. -}
class (Num e, Floating e, Show e) => Field1 e where
  type FieldScalar e :: *
  
instance Field1 CFloat where
  type FieldScalar CFloat = CFloat
  
instance Field1 CDouble where
  type FieldScalar CDouble = CDouble
  
instance Field1 (Complex CFloat) where
  type FieldScalar (Complex CFloat) = CFloat

instance Field1 (Complex CDouble) where
  type FieldScalar (Complex CDouble) = CDouble


