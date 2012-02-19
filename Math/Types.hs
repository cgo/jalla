{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
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

module Math.Types (
                   Field1(..),
                   module Data.Complex,
                   Index,
                   Shape,
                   IndexPair,
                   BLASEnum(..),
                   LAPACKEEnum(..),
                   Order(..),
                   Transpose(..),
                   UpLo(..)) where

import Data.Complex
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign
-- import Data.Convertible

type Index = Int
type Shape = (Index,Index)
type IndexPair = (Index,Index)


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

instance (RealFloat a, Storable a) => Storable (Complex a) where
  -- sizeOf c = s where s = 2 * (sizeOf (f c))
  sizeOf = (2 *) . sizeOf . f
  alignment = alignment . f

  peek p = peek p1 >>= \r -> peek p2 >>= \i -> return $ r :+ i
    where
      p1 = castPtr p
      p2 = advancePtr p1 1

  poke p c = poke p' r >> poke (advancePtr p' 1) i
    where p' = castPtr p
          r = realPart c
          i = imagPart c


class (Num e, Floating e) => Field1 e
instance Field1 CFloat
instance Field1 CDouble
instance Field1 (Complex CFloat)
instance Field1 (Complex CDouble)


