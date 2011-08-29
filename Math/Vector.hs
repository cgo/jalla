{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
    FlexibleInstances, GeneralizedNewtypeDeriving, FlexibleContexts,
    TypeSynonymInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  Math.Vector
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Math.Vector (
    -- * Vector classes
    GVector(..),
    Vector(..),
    CVector(..),

    -- * Functions from indexable
    module Math.Indexable,
    -- * Monadic, efficient vector modification
    VMM,
    -- ** Functions from IMM can be used
    module Math.IMM,
    -- ** Additional functions for CVector types
    createVector,
    modifyVector,
    getVector,
    vectorAdd,
    -- * Inner product
    module Math.InnerProduct
) where

import BLAS.Foreign.BLAS
import BLAS.Foreign.BlasOps
import BLAS.Foreign.LAPACKE
import BLAS.Foreign.LapackeOps
import Math.Internal
import Math.InnerProduct
import Math.IMM
import Math.Indexable

import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign
import Ix
import Data.Complex
import Math.Types
import Control.Monad.State
import Data.Convertible


class (Indexable (vec e) Index e, Field1 e) => GVector vec e where
  vector :: Index -> vec e
  vectorLength :: vec e -> Index
  --(-|) :: vec e -> vec e -> e
  -- (!) :: vec e -> Index -> e

class (BlasOps e, GVector vec e) => CVector vec e where
  vectorAlloc :: Index -> IO (vec e)
  withCVector :: vec e -> (Ptr e -> IO a) -> IO a
  inc :: vec e -> Index


data BlasOps e => Vector e = Vector {vecP :: !(ForeignPtr e),
                                    vecInc :: !Index,
                                    vecLength :: !Index}


vectorAlloc' :: (BlasOps e) => Index -> IO (Vector e)
vectorAlloc' n = mallocArray n >>= newForeignPtr finalizerFree >>= \fp -> return $ Vector fp 1 n


instance (BlasOps e) => GVector Vector e where
    vector n = unsafePerformIO $ vectorAlloc n
    vectorLength = vecLength
    -- v1 -| v2 = innerProduct v1 v2


instance BlasOps e => CVector Vector e where
    vectorAlloc = vectorAlloc'
    withCVector = withForeignPtr . vecP
    inc = vecInc


instance BlasOps e => Indexable (Vector e) Index e where
    v ! i = unsafePerformIO $ unsafeGetElem v i


instance (BlasOps e, BlasOpsReal e, CVector vec e) => InnerProduct (vec e) e where
    innerProduct = innerProductReal

--instance CVector vec CDouble => InnerProduct (vec CDouble) CDouble where
--    innerProduct = innerProductReal

instance (RealFloat e, BlasOpsComplex e, CVector vec (Complex e)) =>
    InnerProduct (vec (Complex e)) (Complex e) where
    innerProduct = innerProductC


innerProductReal :: (BlasOpsReal e, CVector vec e) => vec e -> vec e -> e
innerProductReal v1 v2 | n == n2 = realToFrac $ unsafePerformIO $
    withCVector v1 $ \p1 ->
    withCVector v2 $ \p2 ->
    dot n p1 (inc v1) p2 (inc v2)
        where
            n = vectorLength v1
            n2 = vectorLength v2
innerProductReal _ _ | otherwise = error "innerProduct: vectors must have same length."


innerProductC :: (RealFloat e, BlasOpsComplex e, CVector vec (Complex e)) =>
    vec (Complex e)
    -> vec (Complex e)
    -> Complex e
innerProductC v1 v2 | n == n2 = unsafePerformIO $
    withCVector v1 $ \p1 ->
    withCVector v2 $ \p2 ->
    with (0 :+ 0) $ \pret ->
    dotu_sub n p1 (inc v1) p2 (inc v2) pret >> peek pret
        where
            n = vectorLength v1
            n2 = vectorLength v2
innerProductC _ _ | otherwise = error "innerProduct: vectors must have same length."


{-| Computes v2 <- alpha * v1 + v2. The result is stored in the memory of v2, therefore this is
    unsafe and low level, only for internal use. -}
unsafeVectorAdd :: (BlasOps e, CVector vec e) =>
    e        -- ^ alpha
    -> vec e  -- ^ Vector 1
    -> vec e  -- ^ Vector 2, will be changed in place!
    -> IO ()
unsafeVectorAdd alpha v1 v2 | n == n2 =
    withCVector v1 $ \p1 ->
    withCVector v2 $ \p2 ->
    axpy n alpha p1 (inc v1) p2 (inc v2)
        where
            n = vectorLength v1
            n2 = vectorLength v2
unsafeVectorAdd _ _ _ | otherwise = error "unsafeVectorAdd: Vector lengths must match"


unsafeSetElem :: (BlasOps e, CVector vec e) => vec e -> Index -> e -> IO ()
unsafeSetElem v i e | i >= 0 && i < vectorLength v = withCVector v $
    \p -> let p' = p `plusPtr` (i * sizeOf e * (inc v)) in poke p' e
unsafeSetElem _ _ _ | otherwise = error "unsafeSetElem: out of bounds."

unsafeGetElem :: (BlasOps e, CVector vec e) => vec e -> Index -> IO e
unsafeGetElem v i | i >= 0 && i < vectorLength v = withCVector v $
    \p -> peek p >>= \e1 ->
         let p' = p `plusPtr` (i * sizeOf e1 * (inc v)) in peek p'
unsafeGetElem _ _ | otherwise = error "unsafeGetElem: out of bounds."


unsafeFillVector :: (BlasOps e, CVector vec e) => vec e -> e -> IO ()
unsafeFillVector v e =
    withCVector v $ \p ->
    unsafePtrMap1Inc i (\_ -> e) p n
    where
        i = inc v
        n = vectorLength v


{-| Make a copy of the input vector. Using the cblas_*copy functions. -}
copyVector :: (BlasOps e, CVector vec e) => vec e -> IO (vec e)
copyVector v = vectorAlloc n >>= \ret ->
               withCVector v $ \p ->
               withCVector ret $ \pret ->
               copy n p (inc v) pret (inc ret) >> return ret
               where n = vectorLength v

--------------------------
-- Monadic vector manipulations

type VMMMonad vec e a = StateT (vec e) IO a

newtype VMM s vec e a = VMM { unVMM :: VMMMonad vec e a } deriving Monad

runVMM :: CVector vec e => vec e -> VMM s vec e a -> IO a
runVMM v action = evalStateT action' v
  where
    action' = unVMM action


instance (BlasOps e, CVector vec e) => IMM (VMM s vec e) Index (vec e) e where
--    create   = createVector
--    modify   = modifyVector
--    getO     = getVector
    setElem  = setElem'
    setElems = setElems'
    fill     = fill'
    getElem  = getElem'


createVector :: CVector vec e => Index -> VMM s vec e a -> vec e
createVector n action = unsafePerformIO $
                        vectorAlloc n >>= \mv -> runVMM mv (action >> (VMM get))

getVector :: CVector vec e => VMM s vec e (vec e)
getVector = VMM get

modifyVector :: CVector vec e => vec e -> VMM s vec e a -> vec e
modifyVector v action = unsafePerformIO $
  copyVector v >>= \nv -> runVMM nv (action >> (VMM get))
  where
    n = vectorLength v

{-| Adds alpha * v to the current vector. -}
vectorAdd :: CVector vec e => e -> vec e -> VMM s vec e ()
vectorAdd alpha x = VMM $ (get >>= \v -> liftIO $ unsafeVectorAdd alpha x v)


{-| unsafeSetElem may fail gracefully,
therefore this method may or may not set the element, depending on a successful range check. -}
setElem' :: CVector vec e => Index -> e -> VMM s vec e ()
setElem' i e = VMM $ (get >>= \v -> liftIO $ unsafeSetElem v i e >> return ())

setElems' :: CVector vec e => [(Index,e)] -> VMM s vec e ()
setElems' ies = VMM $ (get >>= \v -> liftIO $ mapM_ (\(i,e) -> unsafeSetElem v i e) ies)

{-| Note: getElem' returns a Maybe. -}
getElem' :: CVector vec e => Index -> VMM s vec e e
getElem' i = VMM $ (get >>= \v -> return $ unsafePerformIO $ unsafeGetElem v i)

fill' :: CVector vec e => e -> VMM s vec e ()
fill' e = VMM $ (get >>= \v -> return $ unsafePerformIO $ unsafeFillVector v e)

--innerProduct :: (BlasOps e, CVector vec e) => vec e -> vec e -> e
--innerProduct = undefined
