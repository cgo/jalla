{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}
-----------------------------------------------------------------------------
--
-- Module      :  BLAS.Foreign.BlasOps
-- Copyright   :  2011 by Christian Gosch
-- License     :  BSD3
--
-- Maintainer  : Christian Gosch <werbung@goschs.de>
-- Stability   : Experimental
-- Portability : GHC only
--
-- | Part of Jalla. This module contains the classes that define
-- BLAS operations, and the instantiations for [Complex] CFloat and CDouble
-- types.
-----------------------------------------------------------------------------

module Numeric.Jalla.Foreign.BlasOps
       (BlasOps(..), BlasOpsReal(..), BlasOpsComplex(..), Storable) where

import Numeric.Jalla.Foreign.BLAS
import Foreign
import Foreign.Storable (Storable)
import Foreign.C.Types
import Numeric.Jalla.Types


convComplex :: (RealFloat a, RealFloat b) => Complex a -> Complex b
convComplex (a :+ b) = (realToFrac a :+ realToFrac b)

{- The following were generated with parseblas.hs and partly hand-tuned afterwards. -}

{- | Low level BLAS operations. Directly call the wrapped BLAS functions.
Things to note: /dot/ returns a /Float/ for the Float instance. If you want the double precision
result, use 'realdot' from the 'BlasOpsReal' class. -}
class (Field1 e, Storable e) => BlasOps e where
  nrm2  :: Int -> Ptr e -> Int -> IO e
  asum  :: Int -> Ptr e -> Int -> IO e
  iamax :: Int -> Ptr e -> Int -> IO CblasIndex
  swap  :: Int -> Ptr e -> Int -> Ptr e -> Int -> IO ()
  copy  :: Int -> Ptr e -> Int -> Ptr e -> Int -> IO ()
  axpy  :: Int -> e -> Ptr e -> Int -> Ptr e -> Int -> IO ()
  scal  :: Int -> e -> Ptr e -> Int -> IO ()
  dot   :: Int -> Ptr e -> Int -> Ptr e -> Int -> IO e
  gemv  :: CblasOrder -> CblasTranspose -> Int -> Int -> e -> Ptr e -> Int -> Ptr e -> Int -> e -> Ptr e -> Int -> IO ()
  gbmv  :: CblasOrder -> CblasTranspose -> Int -> Int -> Int -> Int -> e -> Ptr e -> Int -> Ptr e -> Int -> e -> Ptr e -> Int -> IO ()
  trmv  :: CblasOrder -> CblasUplo -> CblasTranspose -> CblasDiag -> Int -> Ptr e -> Int -> Ptr e -> Int -> IO ()
  tbmv  :: CblasOrder -> CblasUplo -> CblasTranspose -> CblasDiag -> Int -> Int -> Ptr e -> Int -> Ptr e -> Int -> IO ()
  tpmv  :: CblasOrder -> CblasUplo -> CblasTranspose -> CblasDiag -> Int -> Ptr e -> Ptr e -> Int -> IO ()
  trsv  :: CblasOrder -> CblasUplo -> CblasTranspose -> CblasDiag -> Int -> Ptr e -> Int -> Ptr e -> Int -> IO ()
  tbsv  :: CblasOrder -> CblasUplo -> CblasTranspose -> CblasDiag -> Int -> Int -> Ptr e -> Int -> Ptr e -> Int -> IO ()
  tpsv  :: CblasOrder -> CblasUplo -> CblasTranspose -> CblasDiag -> Int -> Ptr e -> Ptr e -> Int -> IO ()
  gemm  :: CblasOrder -> CblasTranspose -> CblasTranspose -> Int -> Int -> Int -> e -> Ptr e -> Int -> Ptr e -> Int -> e -> Ptr e -> Int -> IO ()
  symm  :: CblasOrder -> CblasSide -> CblasUplo -> Int -> Int -> e -> Ptr e -> Int -> Ptr e -> Int -> e -> Ptr e -> Int -> IO ()
  syrk  :: CblasOrder -> CblasUplo -> CblasTranspose -> Int -> Int -> e -> Ptr e -> Int -> e -> Ptr e -> Int -> IO ()
  syr2k :: CblasOrder -> CblasUplo -> CblasTranspose -> Int -> Int -> e -> Ptr e -> Int -> Ptr e -> Int -> e -> Ptr e -> Int -> IO ()
  trmm  :: CblasOrder -> CblasSide -> CblasUplo -> CblasTranspose -> CblasDiag -> Int -> Int -> e -> Ptr e -> Int -> Ptr e -> Int -> IO ()
  trsm  :: CblasOrder -> CblasSide -> CblasUplo -> CblasTranspose -> CblasDiag -> Int -> Int -> e -> Ptr e -> Int -> Ptr e -> Int -> IO ()

class (BlasOps (Complex e)) => BlasOpsComplex e where
  dotu_sub :: Int -> Ptr (Complex e) -> Int -> Ptr (Complex e) -> Int -> Ptr (Complex e) -> IO ()
  dotc_sub :: Int -> Ptr (Complex e) -> Int -> Ptr (Complex e) -> Int -> Ptr (Complex e) -> IO ()
  scal' :: Int -> e -> Ptr (Complex e) -> Int -> IO ()
  hemv :: CblasOrder -> CblasUplo -> Int -> Ptr (Complex e) -> Ptr (Complex e) -> Int -> Ptr (Complex e) -> Int -> Ptr (Complex e) -> Ptr (Complex e) -> Int -> IO ()
  hbmv :: CblasOrder -> CblasUplo -> Int -> Int -> Ptr (Complex e) -> Ptr (Complex e) -> Int -> Ptr (Complex e) -> Int -> Ptr (Complex e) -> Ptr (Complex e) -> Int -> IO ()
  hpmv :: CblasOrder -> CblasUplo -> Int -> Ptr (Complex e) -> Ptr (Complex e) -> Ptr (Complex e) -> Int -> Ptr (Complex e) -> Ptr (Complex e) -> Int -> IO ()
  geru :: CblasOrder -> Int -> Int -> Ptr (Complex e) -> Ptr (Complex e) -> Int -> Ptr (Complex e) -> Int -> Ptr (Complex e) -> Int -> IO ()
  gerc :: CblasOrder -> Int -> Int -> Ptr (Complex e) -> Ptr (Complex e) -> Int -> Ptr (Complex e) -> Int -> Ptr (Complex e) -> Int -> IO ()
  her :: CblasOrder -> CblasUplo -> Int -> e -> Ptr (Complex e) -> Int -> Ptr (Complex e) -> Int -> IO ()
  hpr :: CblasOrder -> CblasUplo -> Int -> e -> Ptr (Complex e) -> Int -> Ptr (Complex e) -> IO ()
  her2 :: CblasOrder -> CblasUplo -> Int -> Ptr (Complex e) -> Ptr (Complex e) -> Int -> Ptr (Complex e) -> Int -> Ptr (Complex e) -> Int -> IO ()
  hpr2 :: CblasOrder -> CblasUplo -> Int -> Ptr (Complex e) -> Ptr (Complex e) -> Int -> Ptr (Complex e) -> Int -> Ptr (Complex e) -> IO ()
  hemm :: CblasOrder -> CblasSide -> CblasUplo -> Int -> Int -> Ptr (Complex e) -> Ptr (Complex e) -> Int -> Ptr (Complex e) -> Int -> Ptr (Complex e) -> Ptr (Complex e) -> Int -> IO ()
  herk :: CblasOrder -> CblasUplo -> CblasTranspose -> Int -> Int -> e -> Ptr (Complex e) -> Int -> e -> Ptr (Complex e) -> Int -> IO ()
  her2k :: CblasOrder -> CblasUplo -> CblasTranspose -> Int -> Int -> Ptr (Complex e) -> Ptr (Complex e) -> Int -> Ptr (Complex e) -> Int -> e -> Ptr (Complex e) -> Int -> IO ()

class (Real e, BlasOps e) => BlasOpsReal e where
  realdot :: Int -> Ptr e -> Int -> Ptr e -> Int -> IO CDouble
  rotg :: Ptr e -> Ptr e -> Ptr e -> Ptr e -> IO ()
  rotmg :: Ptr e -> Ptr e -> Ptr e -> e -> Ptr e -> IO ()
  rot :: Int -> Ptr e -> Int -> Ptr e -> Int -> e -> e -> IO ()
  rotm :: Int -> Ptr e -> Int -> Ptr e -> Int -> Ptr e -> IO ()
  symv :: CblasOrder -> CblasUplo -> Int -> e -> Ptr e -> Int -> Ptr e -> Int -> e -> Ptr e -> Int -> IO ()
  sbmv :: CblasOrder -> CblasUplo -> Int -> Int -> e -> Ptr e -> Int -> Ptr e -> Int -> e -> Ptr e -> Int -> IO ()
  spmv :: CblasOrder -> CblasUplo -> Int -> e -> Ptr e -> Ptr e -> Int -> e -> Ptr e -> Int -> IO ()
  ger :: CblasOrder -> Int -> Int -> e -> Ptr e -> Int -> Ptr e -> Int -> Ptr e -> Int -> IO ()
  syr :: CblasOrder -> CblasUplo -> Int -> e -> Ptr e -> Int -> Ptr e -> Int -> IO ()
  spr :: CblasOrder -> CblasUplo -> Int -> e -> Ptr e -> Int -> Ptr e -> IO ()
  syr2 :: CblasOrder -> CblasUplo -> Int -> e -> Ptr e -> Int -> Ptr e -> Int -> Ptr e -> Int -> IO ()
  spr2 :: CblasOrder -> CblasUplo -> Int -> e -> Ptr e -> Int -> Ptr e -> Int -> Ptr e -> IO ()

instance BlasOps CFloat where
  nrm2 = snrm2
  asum = sasum
  iamax = isamax
  swap = sswap
  copy = scopy
  axpy = saxpy
  scal = sscal
  dot n a inca b incb = fmap realToFrac $ realdot n a inca b incb
  gemv = sgemv
  gbmv = sgbmv
  trmv = strmv
  tbmv = stbmv
  tpmv = stpmv
  trsv = strsv
  tbsv = stbsv
  tpsv = stpsv
  gemm = sgemm
  symm = ssymm
  syrk = ssyrk
  syr2k = ssyr2k
  trmm = strmm
  trsm = strsm

instance BlasOps CDouble where
  nrm2 = dnrm2
  asum = dasum
  iamax = idamax
  swap = dswap
  copy = dcopy
  axpy = daxpy
  scal = dscal
  dot n a inca b incb = fmap realToFrac $ realdot n a inca b incb
  gemv = dgemv
  gbmv = dgbmv
  trmv = dtrmv
  tbmv = dtbmv
  tpmv = dtpmv
  trsv = dtrsv
  tbsv = dtbsv
  tpsv = dtpsv
  gemm = dgemm
  symm = dsymm
  syrk = dsyrk
  syr2k = dsyr2k
  trmm = dtrmm
  trsm = dtrsm

instance BlasOps (Complex CFloat) where
  nrm2 a0 a1 a2 = scnrm2 a0 a1 a2  >>= \a -> return (a :+ 0)
  asum a0 a1 a2 = scasum a0 a1 a2  >>= \a -> return (a :+ 0)
  iamax = icamax
  swap = cswap
  copy = ccopy
  axpy a0 a1 a2 a3 a4 a5 = with (convComplex a1) $ \a1' -> caxpy a0 a1' a2 a3 a4 a5
  scal a0 a1 a2 a3 = with (convComplex a1) $ \a1' -> cscal a0 a1' a2 a3
  dot n a inca b incb = with (0 :+ 0) $ \retp -> dotu_sub n a inca b incb retp >> peek retp
  gemv a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 = with (convComplex a4) $ \a4' -> with (convComplex a9) $ \a9' -> cgemv a0 a1 a2 a3 a4' a5 a6 a7 a8 a9' a10 a11
  gbmv a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 = with (convComplex a6) $ \a6' -> with (convComplex a11) $ \a11' -> cgbmv a0 a1 a2 a3 a4 a5 a6' a7 a8 a9 a10 a11' a12 a13
  trmv = ctrmv
  tbmv = ctbmv
  tpmv = ctpmv
  trsv = ctrsv
  tbsv = ctbsv
  tpsv = ctpsv
  gemm a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 = with (convComplex a6) $ \a6' -> with (convComplex a11) $ \a11' -> cgemm a0 a1 a2 a3 a4 a5 a6' a7 a8 a9 a10 a11' a12 a13
  symm a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 = with (convComplex a5) $ \a5' -> with (convComplex a10) $ \a10' -> csymm a0 a1 a2 a3 a4 a5' a6 a7 a8 a9 a10' a11 a12
  syrk a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = with (convComplex a5) $ \a5' -> with (convComplex a8) $ \a8' -> csyrk a0 a1 a2 a3 a4 a5' a6 a7 a8' a9 a10
  syr2k a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 = with (convComplex a5) $ \a5' -> with (convComplex a10) $ \a10' -> csyr2k a0 a1 a2 a3 a4 a5' a6 a7 a8 a9 a10' a11 a12
  trmm a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 = with (convComplex a7) $ \a7' -> ctrmm a0 a1 a2 a3 a4 a5 a6 a7' a8 a9 a10 a11
  trsm a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 = with (convComplex a7) $ \a7' -> ctrsm a0 a1 a2 a3 a4 a5 a6 a7' a8 a9 a10 a11

instance BlasOps (Complex CDouble) where
  nrm2 a0 a1 a2 = dznrm2 a0 a1 a2  >>= \a -> return (a :+ 0)
  asum a0 a1 a2 = dzasum a0 a1 a2  >>= \a -> return (a :+ 0)
  iamax = izamax
  swap = zswap
  copy = zcopy
  axpy a0 a1 a2 a3 a4 a5 = with (convComplex a1) $ \a1' -> zaxpy a0 a1' a2 a3 a4 a5
  scal a0 a1 a2 a3 = with (convComplex a1) $ \a1' -> zscal a0 a1' a2 a3
  dot n a inca b incb = with (0 :+ 0) $ \retp -> dotu_sub n a inca b incb retp >> peek retp
  gemv a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 = with (convComplex a4) $ \a4' -> with (convComplex a9) $ \a9' -> zgemv a0 a1 a2 a3 a4' a5 a6 a7 a8 a9' a10 a11
  gbmv a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 = with (convComplex a6) $ \a6' -> with (convComplex a11) $ \a11' -> zgbmv a0 a1 a2 a3 a4 a5 a6' a7 a8 a9 a10 a11' a12 a13
  trmv = ztrmv
  tbmv = ztbmv
  tpmv = ztpmv
  trsv = ztrsv
  tbsv = ztbsv
  tpsv = ztpsv
  gemm a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 = with (convComplex a6) $ \a6' -> with (convComplex a11) $ \a11' -> zgemm a0 a1 a2 a3 a4 a5 a6' a7 a8 a9 a10 a11' a12 a13
  symm a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 = with (convComplex a5) $ \a5' -> with (convComplex a10) $ \a10' -> zsymm a0 a1 a2 a3 a4 a5' a6 a7 a8 a9 a10' a11 a12
  syrk a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = with (convComplex a5) $ \a5' -> with (convComplex a8) $ \a8' -> zsyrk a0 a1 a2 a3 a4 a5' a6 a7 a8' a9 a10
  syr2k a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 = with (convComplex a5) $ \a5' -> with (convComplex a10) $ \a10' -> zsyr2k a0 a1 a2 a3 a4 a5' a6 a7 a8 a9 a10' a11 a12
  trmm a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 = with (convComplex a7) $ \a7' -> ztrmm a0 a1 a2 a3 a4 a5 a6 a7' a8 a9 a10 a11
  trsm a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 = with (convComplex a7) $ \a7' -> ztrsm a0 a1 a2 a3 a4 a5 a6 a7' a8 a9 a10 a11

instance BlasOpsComplex CFloat where
  dotu_sub = cdotu_sub
  dotc_sub = cdotc_sub
  scal' = csscal
  hemv = chemv
  hbmv = chbmv
  hpmv = chpmv
  geru = cgeru
  gerc = cgerc
  her = cher
  hpr = chpr
  her2 = cher2
  hpr2 = chpr2
  hemm = chemm
  herk = cherk
  her2k = cher2k

instance BlasOpsComplex CDouble where
  dotu_sub = zdotu_sub
  dotc_sub = zdotc_sub
  scal' = zdscal
  hemv = zhemv
  hbmv = zhbmv
  hpmv = zhpmv
  geru = zgeru
  gerc = zgerc
  her = zher
  hpr = zhpr
  her2 = zher2
  hpr2 = zhpr2
  hemm = zhemm
  herk = zherk
  her2k = zher2k

instance BlasOpsReal CFloat where
  realdot = dsdot
  rotg = srotg
  rotmg = srotmg
  rot = srot
  rotm = srotm
  symv = ssymv
  sbmv = ssbmv
  spmv = sspmv
  ger = sger
  syr = ssyr
  spr = sspr
  syr2 = ssyr2
  spr2 = sspr2

instance BlasOpsReal CDouble where
  realdot = ddot
  rotg = drotg
  rotmg = drotmg
  rot = drot
  rotm = drotm
  symv = dsymv
  sbmv = dsbmv
  spmv = dspmv
  ger = dger
  syr = dsyr
  spr = dspr
  syr2 = dsyr2
  spr2 = dspr2
