{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
    FlexibleInstances, GeneralizedNewtypeDeriving, FlexibleContexts,
    TypeSynonymInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  Math.Vector
-- Copyright   :  2011 by Christian Gosch
-- License     :  BSD3
--
-- Maintainer  : Christian Gosch <werbung@goschs.de>
-- Stability   : Experimental
-- Portability : GHC only
--
-- |
--
-----------------------------------------------------------------------------

module Numeric.Jalla.Vector (
    -- * Data Types
  Vector(..)
  , module Numeric.Jalla.CVector
) where

import Numeric.Jalla.CVector
import Numeric.Jalla.Foreign.BLAS
import Numeric.Jalla.Foreign.BlasOps
import Numeric.Jalla.Foreign.LAPACKE
import Numeric.Jalla.Foreign.LapackeOps
import Numeric.Jalla.Internal
import Numeric.Jalla.IMM
import Numeric.Jalla.Indexable
import Numeric.Jalla.Types

import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign hiding (unsafePerformIO)
import System.IO.Unsafe (unsafePerformIO)
import Data.Ix
import Data.Complex
import Control.Monad.State
import Data.Convertible


{-| Vector is the 'CVector' type that is used in Jalla. 
Somehow Haddock does not want to create documentation for the class instances 
of 'Vector', I try to figure it out. -}
data BlasOps e => Vector e = Vector {vecP :: !(ForeignPtr e),
                                    vecInc :: !Index,
                                    vecLength :: !Index}


vectorAlloc' :: (BlasOps e) => Index -> IO (Vector e)
vectorAlloc' n = mallocForeignPtrArray n >>= \fp -> return $ Vector fp 1 n


instance (BlasOps e) => GVector Vector e where
    vectorLength = vecLength
    -- v1 -| v2 = innerProduct v1 v2


{-| 'CVector' instance for 'Vector'. -}
instance (BlasOps e) => CVector Vector e where
    vectorAlloc = vectorAlloc'
    withCVector = withForeignPtr . vecP
    inc = vecInc


instance BlasOps e => Indexable (Vector e) Index e where
    v ! i = vectorGetElem v i

instance BlasOps e => VectorVector Vector e
instance BlasOps e => VectorScalar Vector e


instance (BlasOps e, Eq e) => Eq (Vector e) where
  a /= b | vectorLength a == vectorLength b = or [x /= y | (x,y) <- zip (vectorList a) (vectorList b)]
        | otherwise = False


instance (BlasOps e, Show e) => Show (Vector e) where
  show v = "listVector " ++ show (vectorList v)


{-| /Num/ instance for a /Vector/. 
The operations are all /element-wise/. There may be the occasional error
by wrongly assuming that /(*)/ returns the inner product, which it doesn't.
This instance is basically only provided to get the + and - operators.
Note that this will /not/ work with 'sum', since 
that assumes it can start with a "0". -}
instance (BlasOps e, Num e) => Num (Vector e) where
  a + b         = modifyVector a $ vectorAdd 1 b
  a - b         = modifyVector a $ vectorAdd (-1) b
  a * b         = vectorBinMap (*) a b
  negate        = vectorMap (* (-1))
  abs           = vectorMap abs
  signum        = vectorMap signum
  fromInteger i = createVector 1 $ setElem 0 (fromIntegral i)
  

instance (BlasOps e, Num e, Fractional e) => Fractional (Vector e) where
  a / b = vectorBinMap (/) a b
  recip = vectorMap recip
  fromRational r = createVector 1 $ setElem 0 (fromRational r)
  
{-| An instance of 'Vector' for 'Floating', for convenience.
    Some of these don't make much sense in some situations,
    but having the trigonometric functions and the like around can be pretty handy. 
    The functions work element-wise. -}
instance (BlasOps e, Num e, Fractional e) => Floating (Vector e) where
  -- | Returns a 1-vector with /pi/ in it.
  pi = createVector 1 $ setElem 0 pi
  exp = vectorMap exp
  sqrt = vectorMap sqrt
  log = vectorMap log
  -- | Takes the element-wise power.
  a ** b = vectorBinMap (**) a b
  -- | Computes 'logBase' the /element-wise/. It may be more useful to simply use /vectorMap (logBase b) v/.
  logBase = vectorBinMap logBase
  sin = vectorMap sin
  tan = vectorMap tan
  cos = vectorMap cos
  asin = vectorMap asin
  atan = vectorMap atan
  acos = vectorMap acos
  sinh = vectorMap sinh
  tanh = vectorMap tanh
  cosh = vectorMap cosh
  asinh = vectorMap asinh
  atanh = vectorMap atanh
  acosh = vectorMap acosh
  
  
{-# SPECIALIZE NOINLINE innerProduct :: Vector CFloat -> Vector CFloat -> CFloat #-}
{-# SPECIALIZE NOINLINE innerProduct :: Vector CDouble -> Vector CDouble -> CDouble #-}
{-# SPECIALIZE NOINLINE innerProduct :: Vector (Complex CFloat) -> Vector (Complex CFloat) -> Complex CFloat #-}
{-# SPECIALIZE NOINLINE innerProduct :: Vector (Complex CDouble) -> Vector (Complex CDouble) -> Complex CDouble #-}

-- Can't do this!
{- SPECIALIZE NOINLINE vectorMap :: (CFloat -> CFloat) -> Vector CFloat -> Vector CFloat #-}
{- SPECIALIZE NOINLINE vectorMap :: (CDouble -> CDouble) -> Vector CDouble -> Vector CDouble #-}
{- SPECIALIZE NOINLINE vectorMap :: (Complex CFloat -> Complex CFloat) -> Vector (Complex CFloat) -> Vector (Complex CFloat) #-}
{- SPECIALIZE NOINLINE vectorMap :: (Complex CDouble -> Complex CDouble) -> Vector (Complex CDouble) -> Vector (Complex CDouble) #-}

{-# SPECIALIZE INLINE unsafeVectorMap :: (CFloat -> CFloat) -> Vector CFloat -> Vector CFloat -> IO () #-}
{-# SPECIALIZE INLINE unsafeVectorMap :: (CDouble -> CDouble) -> Vector CDouble -> Vector CDouble -> IO () #-}
{-# SPECIALIZE INLINE unsafeVectorMap :: (Complex CFloat -> Complex CFloat) -> Vector (Complex CFloat) -> Vector (Complex CFloat) -> IO () #-}
{-# SPECIALIZE INLINE unsafeVectorMap :: (Complex CDouble -> Complex CDouble) -> Vector (Complex CDouble) -> Vector (Complex CDouble) -> IO () #-}

-- Can't do this!
{- SPECIALIZE NOINLINE unsafeVectorAdd :: CFloat -> Vector CFloat -> Vector CFloat -> IO () #-}
{- SPECIALIZE NOINLINE unsafeVectorAdd :: CDouble -> Vector CDouble -> Vector CDouble -> IO () #-}
{- SPECIALIZE NOINLINE unsafeVectorAdd :: Complex CFloat -> Vector (Complex CFloat) -> Vector (Complex CFloat) -> IO () #-}
{- SPECIALIZE NOINLINE unsafeVectorAdd :: Complex CDouble -> Vector (Complex CDouble) -> Vector (Complex CDouble) -> IO () #-}
