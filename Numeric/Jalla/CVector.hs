{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
    FlexibleInstances, GeneralizedNewtypeDeriving, FlexibleContexts,
    TypeSynonymInstances #-}

{-| Functions to work with C-like arrays.
    This is provided to have arrays which work with CBLAS and LAPACKE libraries. -}
module Numeric.Jalla.CVector
       ( 
         -- * Classes
         -- ** Vectors
         GVector(..),
         CVector(..),
         -- ** Vector/vector operations
         VectorVector(..),
         -- ** Vector/scalar operations
         VectorScalar(..),
         -- ** Indexable
         module Numeric.Jalla.Indexable,
         -- * Construction, conversion, modification
         -- ** Monadic, efficient vector modification
         VMM,
         -- getVector,
         createVector,
         modifyVector,
         module Numeric.Jalla.IMM,
         -- ** Vector maps
         vectorAdd,
         vectorMap,
         vectorBinMap,
         -- * Conversion From And To Lists
         listVector,
         vectorList,
         
         -- * Getting Single Values
         vectorGetElem,
         
         -- * Inner product
         innerProduct,
         
         -- * IO Functions
         copyVector,
         
         -- * Unsafe Functions
         unsafeCopyVector,
         unsafeVectorAdd,
         unsafeVectorMap,
         unsafeVectorBinMap,
         
         -- * Re-exported
         CFloat,
         CDouble,
         Complex
       ) where

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


class (Indexable (vec e) Index e, Field1 e) => GVector vec e where
  -- | Creates a vector. Not really useful may be dropped in the future.
  -- vector :: Index -> vec e
  -- | Returns the length of a vector.
  vectorLength :: vec e -> Index
  --(-|) :: vec e -> vec e -> e
  -- (!) :: vec e -> Index -> e

class (BlasOps e, GVector vec e, Show (vec e)) => CVector vec e where
  -- | Allocate a vector of a given length.
  vectorAlloc :: Index -> IO (vec e)
  -- | Operate on a vector with the given IO action. The action gets as parameter a pointer to the array.
  withCVector :: vec e -> (Ptr e -> IO a) -> IO a
  -- | Returns the increment per element for this vector (like the /inc/ arguments for BLAS). For contiguous storage, this would simply be 1.
  inc :: vec e -> Index


infixl 7 ||*
infixl 6 ||+,||-

{-| Vector/vector operations. -}
class (CVector vec e) => VectorVector vec e where
  -- | Vector addition
  (||+) :: vec e -> vec e -> vec e
  v1 ||+ v2 = modifyVector v1 $ vectorAdd 1 v2
  -- | Vector subtraction
  (||-) :: vec e -> vec e -> vec e
  v1 ||- v2 = modifyVector v1 $ vectorAdd (-1) v2
  -- | Dot product
  (||*) :: vec e -> vec e -> e
  v1 ||* v2 = innerProduct v1 v2


innerProduct :: (BlasOps e, CVector vec e) => vec e -> vec e -> e
innerProduct v1 v2 | n == n2 = unsafePerformIO $
    withCVector v1 $ \p1 ->
    withCVector v2 $ \p2 ->
    dot n p1 (inc v1) p2 (inc v2)
        where
            n = vectorLength v1
            n2 = vectorLength v2
innerProduct _ _ | otherwise = error "innerProduct: vectors must have same length."


innerProductReal :: (BlasOpsReal e, CVector vec e) => vec e -> vec e -> CDouble
innerProductReal v1 v2 | n == n2 = realToFrac $ unsafePerformIO $
    withCVector v1 $ \p1 ->
    withCVector v2 $ \p2 ->
    realdot n p1 (inc v1) p2 (inc v2)
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


infixl 7 |.*,|./
infixl 6 |.+,|.-

{-| Vector manipulations by a scalar. -}
class (CVector vec e) => VectorScalar vec e where
  (|.*) :: vec e -> e -> vec e
  a |.* b = vectorMap (*b) a
  (|./) :: vec e -> e -> vec e
  a |./ b = vectorMap (/b) a
  (|.+) :: vec e -> e -> vec e
  a |.+ b = vectorMap (+b) a
  (|.-) :: vec e -> e -> vec e
  a |.- b = vectorMap ((-)b) a


vectorGetElem :: CVector vec e => vec e -> Index -> e
vectorGetElem v i = unsafePerformIO $ unsafeGetElem v i


vectorList :: GVector vec e => vec e -> [e]
vectorList v = map (v !) [0..n-1] where n = vectorLength v

listVector :: (CVector vec e) => [e] -> vec e
listVector es = createVector n $ setElems ies
  where n = length es
        ies = zip [0..n-1] es

{-| Maps a unary function over the elements of a vector and returns the resulting vector. -}
vectorMap :: (CVector vec1 e1, CVector vec2 e2) => (e1 -> e2) -> vec1 e1 -> vec2 e2
vectorMap f v1 = unsafePerformIO $
                 vectorAlloc n >>= \v2 -> unsafeVectorMap f v1 v2 >> return v2
  where n = vectorLength v1
{-# NOINLINE vectorMap #-}                                          



{-| Maps a binary function to the elements of two vectors and returns the resulting vector. -}
vectorBinMap :: (CVector vec1 e1, CVector vec2 e2, CVector vec3 e3) => 
                (e1 -> e2 -> e3)    -- ^ The function /f/ to map.
                -> vec1 e1 -- ^ The first input vector /v1/ for /f/.
                -> vec2 e2 -- ^ The second input vector /v2/ for /f/.
                -> vec3 e3 -- ^ The result vector. It will have length min(l1,l2), where l1,l2 are the lengths of /v1/ and /v2/.
vectorBinMap f v1 v2 = unsafePerformIO $
                       vectorAlloc n >>= \v3 -> unsafeVectorBinMap f v1 v2 v3 >> return v3
  where n = min (vectorLength v1) (vectorLength v2)




{-| Make a copy of the input vector. Using the cblas_*copy functions. -}
copyVector :: (BlasOps e, CVector vec e, CVector vec2 e) => vec e -> IO (vec2 e)
copyVector v = vectorAlloc n >>= \ret ->
               withCVector v $ \p ->
               withCVector ret $ \pret ->
               copy n p (inc v) pret (inc ret) >> return ret
               where n = vectorLength v



-------------------------------
-- Monadic vector manipulations
-------------------------------

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
getElem' i = VMM $ get >>= \v -> liftIO (unsafeGetElem v i)

fill' :: CVector vec e => e -> VMM s vec e ()
fill' e = VMM $ get >>= \v -> liftIO (unsafeFillVector v e)



---------------------------------------------------------------------------------------
-- Unsafe functions.
---------------------------------------------------------------------------------------

unsafeVectorMap :: (CVector vec1 e1, CVector vec2 e2) => (e1 -> e2) -> vec1 e1 -> vec2 e2 -> IO ()
unsafeVectorMap f v1 v2 = 
  withCVector v1 $ \v1p ->
  withCVector v2 $ \v2p ->
  unsafePtrMapInc i1 i2 f v1p v2p n
  where
    i1 = inc v1
    i2 = inc v2
    n = min (vectorLength v1) (vectorLength v2)
{-# INLINABLE unsafeVectorMap #-}

{-| Copies from one vector to the other, in-place and therefore unsafely.
Uses the BLAS 'copy' function. /min (vectorLength src) (vectorlength dest)/
elements are copied from the first to the second vector. -}
unsafeCopyVector :: (CVector vec e, CVector vec2 e) => 
                    vec e    -- ^ The source vector.
                    -> vec2 e -- ^ The destination vector.
                    -> IO ()
unsafeCopyVector src dest =
  withCVector src $ \srcp -> 
  withCVector dest $ \destp ->
  copy n srcp (inc src) destp (inc dest)
  where n = min (vectorLength src) (vectorLength dest)

unsafeVectorBinMap :: (CVector vec1 e1, CVector vec2 e2, CVector vec3 e3) => 
                      (e1 -> e2 -> e3) 
                      -> vec1 e1 
                      -> vec2 e2 
                      -> vec3 e3 
                      -> IO ()
unsafeVectorBinMap f v1 v2 v3 = 
  withCVector v1 $ \v1p ->
  withCVector v2 $ \v2p ->
  withCVector v3 $ \v3p -> 
  unsafe2PtrMapInc i1 i2 i3 f v1p v2p v3p n
  where
    i1 = inc v1
    i2 = inc v2
    i3 = inc v3
    n = minimum [(vectorLength v1), (vectorLength v2), vectorLength v3]


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
unsafeVectorAdd _ v1 v2 | otherwise = error $ "unsafeVectorAdd: Vector lengths must match, when adding " ++ show v1 ++ "\nand\n" ++ show v2 
{-# NOINLINE unsafeVectorAdd #-}

unsafeSetElem :: (BlasOps e, CVector vec e) => vec e -> Index -> e -> IO ()
unsafeSetElem v i e | i >= 0 && i < vectorLength v = withCVector v $
    \p -> let p' = p `plusPtr` (i * sizeOf e * (inc v)) in poke p' e
unsafeSetElem _ _ _ | otherwise = error "unsafeSetElem: out of bounds."

unsafeGetElem :: (BlasOps e, CVector vec e) => vec e -> Index -> IO e
unsafeGetElem v i | i >= 0 && i < vectorLength v = withCVector v $ \p -> do
    e1 <- peek p
    let p' = p `plusPtr` (i * sizeOf e1 * (inc v))
    peek p'
unsafeGetElem _ _ | otherwise = error "unsafeGetElem: out of bounds."


unsafeFillVector :: (BlasOps e, CVector vec e) => vec e -> e -> IO ()
unsafeFillVector v e =
    withCVector v $ \p ->
    unsafePtrMap1Inc i (const e) p n
  where
    -- Using /f/ instead of the unsafePtrMap1Inc should be /slightly/ more efficient,
    -- but it is a good idea to have such dirty functions in a central place.
--    f _ _ 0 = return ()
--    f i p n = poke p e >> f i (advancePtr p i) (n - 1) where {p' = advancePtr p i; n' = n - 1 }
    i = inc v
    n = vectorLength v