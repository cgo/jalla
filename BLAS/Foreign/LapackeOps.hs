{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}
-----------------------------------------------------------------------------
--
-- Module      :  BLAS.Foreign.LapackeOps
-- Copyright   :  2011 by Christian Gosch
-- License     :  BSD3
--
-- Maintainer  : Christian Gosch <werbung@goschs.de>
-- Stability   : Experimental
-- Portability : GHC only
--
-- | Part of Jalla. This module contains the classes that define
-- LAPACKE operations, and the instantiations for [Complex] CFloat and CDouble
-- types. Some LAPACKE functions don't really fit the rest, namely tgex tgsen.
-- They are, however, probably not needed.
-----------------------------------------------------------------------------

module BLAS.Foreign.LapackeOps 
       (LapackeOps(..), LapackeOpsReal(..), LapackeOpsComplex(..)) where

import BLAS.Foreign.BLAS
import BLAS.Foreign.LAPACKE
import Foreign
import Foreign.C.Types
import Math.Types

instance LAPACKEEnum UpLo CChar where
  toLapacke Up = (toEnum . fromEnum) 'u'
  toLapacke Lo = (toEnum . fromEnum) 'l'
  fromLapacke c | c == (toEnum . fromEnum) 'u' = Up
                | c == (toEnum . fromEnum) 'l' = Lo

-- (ZU TUN) erledigt: e/he durch e ersetzen; reine skalare durch se ersetzen
class (Field1 e, Field1 se) => LapackeOps e se | e -> se where
  gbequ :: Int -> Int -> Int -> Int -> Int -> Ptr e -> Int -> Ptr se -> Ptr se -> Ptr se -> Ptr se -> Ptr se -> IO Int
  gbequb :: Int -> Int -> Int -> Int -> Int -> Ptr e -> Int -> Ptr se -> Ptr se -> Ptr se -> Ptr se -> Ptr se -> IO Int
  gbsv :: Int -> Int -> Int -> Int -> Int -> Ptr e -> Int -> Ptr CInt -> Ptr e -> Int -> IO Int
  gbtrf :: Int -> Int -> Int -> Int -> Int -> Ptr e -> Int -> Ptr CInt -> IO Int
  gebrd :: Int -> Int -> Int -> Ptr e -> Int -> Ptr se -> Ptr se -> Ptr e -> Ptr e -> IO Int
  geequ :: Int -> Int -> Int -> Ptr e -> Int -> Ptr se -> Ptr se -> Ptr se -> Ptr se -> Ptr se -> IO Int
  geequb :: Int -> Int -> Int -> Ptr e -> Int -> Ptr se -> Ptr se -> Ptr se -> Ptr se -> Ptr se -> IO Int
  gehrd :: Int -> Int -> Int -> Int -> Ptr e -> Int -> Ptr e -> IO Int
  gelqf :: Int -> Int -> Int -> Ptr e -> Int -> Ptr e -> IO Int
  gelsd :: Int -> Int -> Int -> Int -> Ptr e -> Int -> Ptr e -> Int -> Ptr se -> se -> Ptr CInt -> IO Int
  gelss :: Int -> Int -> Int -> Int -> Ptr e -> Int -> Ptr e -> Int -> Ptr se -> se -> Ptr CInt -> IO Int
  gelsy :: Int -> Int -> Int -> Int -> Ptr e -> Int -> Ptr e -> Int -> Ptr CInt -> se -> Ptr CInt -> IO Int
  geqlf :: Int -> Int -> Int -> Ptr e -> Int -> Ptr e -> IO Int
  geqp3 :: Int -> Int -> Int -> Ptr e -> Int -> Ptr CInt -> Ptr e -> IO Int
  geqpf :: Int -> Int -> Int -> Ptr e -> Int -> Ptr CInt -> Ptr e -> IO Int
  geqrf :: Int -> Int -> Int -> Ptr e -> Int -> Ptr e -> IO Int
--  geqrfp :: Int -> Int -> Int -> Ptr e -> Int -> Ptr e -> IO Int
  gerqf :: Int -> Int -> Int -> Ptr e -> Int -> Ptr e -> IO Int
  gesv :: Int -> Int -> Int -> Ptr e -> Int -> Ptr CInt -> Ptr e -> Int -> IO Int
  getrf :: Int -> Int -> Int -> Ptr e -> Int -> Ptr CInt -> IO Int
  getri :: Int -> Int -> Ptr e -> Int -> Ptr CInt -> IO Int
  ggglm :: Int -> Int -> Int -> Int -> Ptr e -> Int -> Ptr e -> Int -> Ptr e -> Ptr e -> Ptr e -> IO Int
  gglse :: Int -> Int -> Int -> Int -> Ptr e -> Int -> Ptr e -> Int -> Ptr e -> Ptr e -> Ptr e -> IO Int
  ggqrf :: Int -> Int -> Int -> Int -> Ptr e -> Int -> Ptr e -> Ptr e -> Int -> Ptr e -> IO Int
  ggrqf :: Int -> Int -> Int -> Int -> Ptr e -> Int -> Ptr e -> Ptr e -> Int -> Ptr e -> IO Int
  gtsv :: Int -> Int -> Int -> Ptr e -> Ptr e -> Ptr e -> Ptr e -> Int -> IO Int
  gttrf :: Int -> Ptr e -> Ptr e -> Ptr e -> Ptr e -> Ptr CInt -> IO Int
  poequ :: Int -> Int -> Ptr e -> Int -> Ptr se -> Ptr se -> Ptr se -> IO Int
  poequb :: Int -> Int -> Ptr e -> Int -> Ptr se -> Ptr se -> Ptr se -> IO Int
  ptcon :: Int -> Ptr se -> Ptr e -> se -> Ptr se -> IO Int
  ptsv :: Int -> Int -> Int -> Ptr se -> Ptr e -> Ptr e -> Int -> IO Int
  pttrf :: Int -> Ptr se -> Ptr e -> IO Int
  stein :: Int -> Int -> Ptr se -> Ptr se -> Int -> Ptr se -> Ptr CInt -> Ptr CInt -> Ptr e -> Int -> Ptr CInt -> IO Int
  -- Matrix reordering operations -- are these really needed from Haskell?
  -- They have some differing parameters, therefore don't fit in this type class unhandled.
  --tgexc :: Int -> Int -> Int -> Int -> Ptr e -> Int -> Ptr e -> Int -> Ptr e -> Int -> Ptr e -> Int -> Ptr CInt -> Ptr CInt -> IO Int
  --tgsen :: Int -> Int -> Int -> Int -> Ptr CInt -> Int -> Ptr e -> Int -> Ptr e -> Int -> Ptr e -> Ptr e -> Ptr e -> Ptr e -> Int -> Ptr e -> Int -> Ptr CInt -> Ptr e -> Ptr e -> Ptr e -> IO Int
  tzrzf :: Int -> Int -> Int -> Ptr e -> Int -> Ptr e -> IO Int

class (LapackeOps (Complex e) e) => LapackeOpsComplex e where
  unghr :: Int -> Int -> Int -> Int -> Ptr (Complex e) -> Int -> Ptr (Complex e) -> IO Int
  unglq :: Int -> Int -> Int -> Int -> Ptr (Complex e) -> Int -> Ptr (Complex e) -> IO Int
  ungql :: Int -> Int -> Int -> Int -> Ptr (Complex e) -> Int -> Ptr (Complex e) -> IO Int
  ungqr :: Int -> Int -> Int -> Int -> Ptr (Complex e) -> Int -> Ptr (Complex e) -> IO Int
  ungrq :: Int -> Int -> Int -> Int -> Ptr (Complex e) -> Int -> Ptr (Complex e) -> IO Int

class (LapackeOps e se) => LapackeOpsReal e se where
  orghr :: Int -> Int -> Int -> Int -> Ptr e -> Int -> Ptr e -> IO Int
  orglq :: Int -> Int -> Int -> Int -> Ptr e -> Int -> Ptr e -> IO Int
  orgql :: Int -> Int -> Int -> Int -> Ptr e -> Int -> Ptr e -> IO Int
  orgqr :: Int -> Int -> Int -> Int -> Ptr e -> Int -> Ptr e -> IO Int
  orgrq :: Int -> Int -> Int -> Int -> Ptr e -> Int -> Ptr e -> IO Int
  ptrfs :: Int -> Int -> Int -> Ptr e -> Ptr e -> Ptr e -> Ptr e -> Ptr e -> Int -> Ptr e -> Int -> Ptr e -> Ptr e -> IO Int
  pttrs :: Int -> Int -> Int -> Ptr e -> Ptr e -> Ptr e -> Int -> IO Int
  sterf :: Int -> Ptr e -> Ptr e -> IO Int

instance LapackeOps CFloat CFloat where
  gbequ = sgbequ
  gbequb = sgbequb
  gbsv = sgbsv
  gbtrf = sgbtrf
  gebrd = sgebrd
  geequ = sgeequ
  geequb = sgeequb
  gehrd = sgehrd
  gelqf = sgelqf
  gelsd = sgelsd
  gelss = sgelss
  gelsy = sgelsy
  geqlf = sgeqlf
  geqp3 = sgeqp3
  geqpf = sgeqpf
  geqrf = sgeqrf
--  geqrfp = sgeqrfp
  gerqf = sgerqf
  gesv = sgesv
  getrf = sgetrf
  getri = sgetri
  ggglm = sggglm
  gglse = sgglse
  ggqrf = sggqrf
  ggrqf = sggrqf
  gtsv = sgtsv
  gttrf = sgttrf
  poequ = spoequ
  poequb = spoequb
  ptcon = sptcon
  ptsv = sptsv
  pttrf = spttrf
  stein = sstein
  --tgexc = stgexc
  --tgsen = stgsen
  tzrzf = stzrzf

instance LapackeOps CDouble CDouble where
  gbequ = dgbequ
  gbequb = dgbequb
  gbsv = dgbsv
  gbtrf = dgbtrf
  gebrd = dgebrd
  geequ = dgeequ
  geequb = dgeequb
  gehrd = dgehrd
  gelqf = dgelqf
  gelsd = dgelsd
  gelss = dgelss
  gelsy = dgelsy
  geqlf = dgeqlf
  geqp3 = dgeqp3
  geqpf = dgeqpf
  geqrf = dgeqrf
  -- geqrfp = dgeqrfp
  gerqf = dgerqf
  gesv = dgesv
  getrf = dgetrf
  getri = dgetri
  ggglm = dggglm
  gglse = dgglse
  ggqrf = dggqrf
  ggrqf = dggrqf
  gtsv = dgtsv
  gttrf = dgttrf
  poequ = dpoequ
  poequb = dpoequb
  ptcon = dptcon
  ptsv = dptsv
  pttrf = dpttrf
  stein = dstein
  --tgexc = dtgexc
  --tgsen = dtgsen
  tzrzf = dtzrzf

instance LapackeOps (Complex CFloat) CFloat where
  gbequ = cgbequ
  gbequb = cgbequb
  gbsv = cgbsv
  gbtrf = cgbtrf
  gebrd = cgebrd
  geequ = cgeequ
  geequb = cgeequb
  gehrd = cgehrd
  gelqf = cgelqf
  gelsd = cgelsd
  gelss = cgelss
  gelsy = cgelsy
  geqlf = cgeqlf
  geqp3 = cgeqp3
  geqpf = cgeqpf
  geqrf = cgeqrf
--  geqrfp = cgeqrfp
  gerqf = cgerqf
  gesv = cgesv
  getrf = cgetrf
  getri = cgetri
  ggglm = cggglm
  gglse = cgglse
  ggqrf = cggqrf
  ggrqf = cggrqf
  gtsv = cgtsv
  gttrf = cgttrf
  poequ = cpoequ
  poequb = cpoequb
  ptcon = cptcon
  ptsv = cptsv
  pttrf = cpttrf
  stein = cstein
  --tgexc = ctgexc
  --tgsen = ctgsen
  tzrzf = ctzrzf

instance LapackeOps (Complex CDouble) CDouble where
  gbequ = zgbequ
  gbequb = zgbequb
  gbsv = zgbsv
  gbtrf = zgbtrf
  gebrd = zgebrd
  geequ = zgeequ
  geequb = zgeequb
  gehrd = zgehrd
  gelqf = zgelqf
  gelsd = zgelsd
  gelss = zgelss
  gelsy = zgelsy
  geqlf = zgeqlf
  geqp3 = zgeqp3
  geqpf = zgeqpf
  geqrf = zgeqrf
--  geqrfp = zgeqrfp
  gerqf = zgerqf
  gesv = zgesv
  getrf = zgetrf
  getri = zgetri
  ggglm = zggglm
  gglse = zgglse
  ggqrf = zggqrf
  ggrqf = zggrqf
  gtsv = zgtsv
  gttrf = zgttrf
  poequ = zpoequ
  poequb = zpoequb
  ptcon = zptcon
  ptsv = zptsv
  pttrf = zpttrf
  stein = zstein
  --tgexc = ztgexc
  --tgsen = ztgsen
  tzrzf = ztzrzf

instance LapackeOpsComplex CFloat where
  unghr = cunghr
  unglq = cunglq
  ungql = cungql
  ungqr = cungqr
  ungrq = cungrq

instance LapackeOpsComplex CDouble where
  unghr = zunghr
  unglq = zunglq
  ungql = zungql
  ungqr = zungqr
  ungrq = zungrq

instance LapackeOpsReal CFloat CFloat where
  orghr = sorghr
  orglq = sorglq
  orgql = sorgql
  orgqr = sorgqr
  orgrq = sorgrq
  ptrfs = sptrfs
  pttrs = spttrs
  sterf = ssterf

instance LapackeOpsReal CDouble CDouble where
  orghr = dorghr
  orglq = dorglq
  orgql = dorgql
  orgqr = dorgqr
  orgrq = dorgrq
  ptrfs = dptrfs
  pttrs = dpttrs
  sterf = dsterf