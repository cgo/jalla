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

module Jalla.Vector (
    -- * Classes
    -- ** Vectors
    GVector(..),
    CVector(..),
    -- ** Vector/vector operations
    VectorVector(..),
    -- ** Vector/scalar operations
    VectorScalar(..),
    -- ** Indexable
    module Jalla.Indexable,
    -- * Data Types
    Vector(..),
    
    -- * Construction, conversion, modification
    -- ** Monadic, efficient vector modification
    VMM,
    getVector,
    createVector,
    modifyVector,
    module Jalla.IMM,
    -- ** Vector maps
    vectorAdd,
    vectorMap,
    vectorBinMap,
    -- * Conversion From And To Lists
    listVector,
    vectorList,
    
    -- * IO Functions
    copyVector,
    
    -- * Low-level, unsafe functions
    unsafeVectorAdd,
    unsafeCopyVector,
    
    -- * Re-exported
    CFloat,
    CDouble,
    Complex
) where


import Jalla.BLAS.Foreign.BLAS
import Jalla.BLAS.Foreign.BlasOps
import Jalla.BLAS.Foreign.LAPACKE
import Jalla.BLAS.Foreign.LapackeOps
import Jalla.Internal
import Jalla.IMM
import Jalla.Indexable
import Jalla.Types

import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign
import Data.Ix
import Data.Complex
import Control.Monad.State
import Data.Convertible


class (Indexable (vec e) Index e, Field1 e) => GVector vec e where
  vector :: Index -> vec e
  vectorLength :: vec e -> Index
  --(-|) :: vec e -> vec e -> e
  -- (!) :: vec e -> Index -> e

class (BlasOps e, GVector vec e, Show (vec e)) => CVector vec e where
  vectorAlloc :: Index -> IO (vec e)
  withCVector :: vec e -> (Ptr e -> IO a) -> IO a
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
{-# SPECIALIZE NOINLINE innerProduct :: Vector CFloat -> Vector CFloat -> CFloat #-}
{-# SPECIALIZE NOINLINE innerProduct :: Vector CDouble -> Vector CDouble -> CDouble #-}
{-# SPECIALIZE NOINLINE innerProduct :: Vector (Complex CFloat) -> Vector (Complex CFloat) -> Complex CFloat #-}
{-# SPECIALIZE NOINLINE innerProduct :: Vector (Complex CDouble) -> Vector (Complex CDouble) -> Complex CDouble #-}


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

{-| Vector is the 'CVector' type that is used in Jalla. 
Somehow Haddock does not want to create documentation for the class instances 
of 'Vector', I try to figure it out. -}
data BlasOps e => Vector e = Vector {vecP :: !(ForeignPtr e),
                                    vecInc :: !Index,
                                    vecLength :: !Index}


vectorAlloc' :: (BlasOps e) => Index -> IO (Vector e)
vectorAlloc' n = mallocForeignPtrArray n >>= \fp -> return $ Vector fp 1 n


instance (BlasOps e) => GVector Vector e where
    vector n = unsafePerformIO $ vectorAlloc n
    vectorLength = vecLength
    -- v1 -| v2 = innerProduct v1 v2


{-| 'CVector' instance for 'Vector'. -}
instance (BlasOps e) => CVector Vector e where
    vectorAlloc = vectorAlloc'
    withCVector = withForeignPtr . vecP
    inc = vecInc


instance BlasOps e => Indexable (Vector e) Index e where
    v ! i = unsafePerformIO $ unsafeGetElem v i

instance BlasOps e => VectorVector Vector e
instance BlasOps e => VectorScalar Vector e

vectorList :: GVector vec e => vec e -> [e]
vectorList v = map (v !) [0..n-1] where n = vectorLength v

listVector :: (CVector vec e) => [e] -> vec e
listVector es = createVector n $ setElems ies
  where n = length es
        ies = zip [0..n-1] es


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
  
  
  

{-| Maps a unary function over the elements of a vector and returns the resulting vector. -}
vectorMap :: (CVector vec1 e1, CVector vec2 e2) => (e1 -> e2) -> vec1 e1 -> vec2 e2
vectorMap f v1 = unsafePerformIO $
                 vectorAlloc n >>= \v2 -> unsafeVectorMap f v1 v2 >> return v2
  where n = vectorLength v1
{-# NOINLINE vectorMap #-}                                          
{-# SPECIALIZE NOINLINE vectorMap :: (CFloat -> CFloat) -> Vector CFloat -> Vector CFloat #-}
{-# SPECIALIZE NOINLINE vectorMap :: (CDouble -> CDouble) -> Vector CDouble -> Vector CDouble #-}
{-# SPECIALIZE NOINLINE vectorMap :: (Complex CFloat -> Complex CFloat) -> Vector (Complex CFloat) -> Vector (Complex CFloat) #-}
{-# SPECIALIZE NOINLINE vectorMap :: (Complex CDouble -> Complex CDouble) -> Vector (Complex CDouble) -> Vector (Complex CDouble) #-}

unsafeVectorMap :: (CVector vec1 e1, CVector vec2 e2) => (e1 -> e2) -> vec1 e1 -> vec2 e2 -> IO ()
unsafeVectorMap f v1 v2 = 
  withCVector v1 $ \v1p ->
  withCVector v2 $ \v2p ->
  unsafePtrMapInc i1 i2 f v1p v2p n
  where
    i1 = inc v1
    i2 = inc v2
    n = min (vectorLength v1) (vectorLength v2)
{-# SPECIALIZE INLINE unsafeVectorMap :: (CFloat -> CFloat) -> Vector CFloat -> Vector CFloat -> IO () #-}
{-# SPECIALIZE INLINE unsafeVectorMap :: (CDouble -> CDouble) -> Vector CDouble -> Vector CDouble -> IO () #-}
{-# SPECIALIZE INLINE unsafeVectorMap :: (Complex CFloat -> Complex CFloat) -> Vector (Complex CFloat) -> Vector (Complex CFloat) -> IO () #-}
{-# SPECIALIZE INLINE unsafeVectorMap :: (Complex CDouble -> Complex CDouble) -> Vector (Complex CDouble) -> Vector (Complex CDouble) -> IO () #-}


{-| Maps a binary function to the elements of two vectors and returns the resulting vector. -}
vectorBinMap :: (CVector vec1 e1, CVector vec2 e2, CVector vec3 e3) => 
                (e1 -> e2 -> e3)    -- ^ The function /f/ to map.
                -> vec1 e1 -- ^ The first input vector /v1/ for /f/.
                -> vec2 e2 -- ^ The second input vector /v2/ for /f/.
                -> vec3 e3 -- ^ The result vector. It will have length min(l1,l2), where l1,l2 are the lengths of /v1/ and /v2/.
vectorBinMap f v1 v2 = unsafePerformIO $
                       vectorAlloc n >>= \v3 -> unsafeVectorBinMap f v1 v2 v3 >> return v3
  where n = min (vectorLength v1) (vectorLength v2)


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
{-# SPECIALIZE NOINLINE unsafeVectorAdd :: CFloat -> Vector CFloat -> Vector CFloat -> IO () #-}
{-# SPECIALIZE NOINLINE unsafeVectorAdd :: CDouble -> Vector CDouble -> Vector CDouble -> IO () #-}
{-# SPECIALIZE NOINLINE unsafeVectorAdd :: Complex CFloat -> Vector (Complex CFloat) -> Vector (Complex CFloat) -> IO () #-}
{-# SPECIALIZE NOINLINE unsafeVectorAdd :: Complex CDouble -> Vector (Complex CDouble) -> Vector (Complex CDouble) -> IO () #-}

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


{-| Make a copy of the input vector. Using the cblas_*copy functions. -}
copyVector :: (BlasOps e, CVector vec e, CVector vec2 e) => vec e -> IO (vec2 e)
copyVector v = vectorAlloc n >>= \ret ->
               withCVector v $ \p ->
               withCVector ret $ \pret ->
               copy n p (inc v) pret (inc ret) >> return ret
               where n = vectorLength v


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
getElem' i = VMM $ get >>= \v -> liftIO (unsafeGetElem v i)

fill' :: CVector vec e => e -> VMM s vec e ()
fill' e = VMM $ get >>= \v -> liftIO (unsafeFillVector v e)

