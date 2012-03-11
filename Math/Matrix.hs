{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleContexts, RankNTypes, 
ExistentialQuantification, ScopedTypeVariables, TypeFamilies #-}

-----------------------------------------------------------------------------
--
-- Module      :  Math.Matrix
-- Copyright   :  2011 by Christian Gosch
-- License     :  BSD3
--
-- Maintainer  : Christian Gosch <werbung@goschs.de>
-- Stability   : Experimental
-- Portability : GHC only
--
-- | This is the matrix module of Jalla.
-- 
-----------------------------------------------------------------------------


module Math.Matrix
       (
         -- * Classes
         -- ** Matrices
         GMatrix(..),
         CMatrix(..),
         shapeTrans,
         -- ** Matrix/Matrix Operations
         MatrixMatrix(..),
         -- ** Matrix/Vector Operations
         MatrixVector(..),
         -- ** Matrix/Scalar Operations
         MatrixScalar(..),
        -- ** Indexable
         module Math.Indexable,
         -- * Data types
         Matrix,
         Order(..),
         Transpose(..),
         RefVector,

        -- * Construction, Conversion, Manipulation
        -- ** Manipulation Monad and Functions
        MMM,
        createMatrix,
        modifyMatrix,
        -- getMatrix,
        setDiag,
        setRow,
        setColumn,
        setBlock,
        fillBlock,
        scaleRow,
        scaleColumn,
        refRow,
        refColumn,
        -- ** Maps over 'CMatrix'
        matrixMap,
        matrixBinMap,
        -- ** Conversions To And From Lists
        matrixList,
        matrixLists,
        listMatrix,
        matrixAssocs,
        gmatrixAssocs,
        -- ** Copying Rows and Columns
        row,
        column,
        rows,
        columns,
         -- ** Functions From IMM Can Be Used
        module Math.IMM,

        -- * Printing Matrices
        prettyPrintMatrix,
        prettyPrintMatrixIO,
        
        -- * CMatrix Linear Algebra Functions
        -- ** Solving Linear Systems
        solveLinearSystem,
        -- ** Inversion
        invert,
        pseudoInverse,
        -- ** Special Matrices And Operations
        idMatrix,
        matrixMultDiag,
        -- ** SVD
        svd,
        SVD(..),
        SVDOpt(..),
        SVDU(..),
        SVDVT(..),
        
        -- * Generating and Checking Indices
        checkIndex,
        inMatrixRange,
        diagIndices,

        -- * Low Level IO Matrix Functions
        matrixAlloc',
        matrixElem,
        matrixMult,

        -- * Unsafe manipulations. 
        -- Do not use these unless you know what you are doing.
        -- These may change without notice and may be removed from the visible interface.
        unsafeMatrixSetElem,
        unsafeMatrixMult,
        unsafeMatrixFill,
        unsafeMatrixCopy,
        unsafeSolveLinearSystem,
        unsafeSVD,
        unsafeMatrixMap,
        unsafeMatrixBinMap,

        withCMatrixRow,
        withCMatrixColumn,
        
        -- * Re-exported
        CFloat,
        CDouble,
        Complex
        ) where

import BLAS.Foreign.BLAS
import BLAS.Foreign.BlasOps
import BLAS.Foreign.LAPACKE
import BLAS.Foreign.LapackeOps
import Math.Internal
import Math.IMM
import Math.Vector
import Math.Indexable

import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign
import Data.Ix
import Data.Complex
import Data.List (partition)
import Data.Maybe (fromJust)
import Math.Types
import Control.Monad.State
import Data.Convertible

{-
   TODO: Storage type zu CMatrix Typ hinzufuegen; Funktionen wie "unsafeSetElem",
     "unsafeGetElem", "matrixMult", "multiplyAdd",... zu
     CMatrix hinzufuegen; Instanzen fuer jeden storage Typ fuer Matrix. -}

instance BLASEnum Order CblasOrder where
  toBlas RowMajor        = CblasRowMajor
  toBlas ColumnMajor     = CblasColMajor
  fromBlas CblasRowMajor = RowMajor
  fromBlas CblasColMajor = ColumnMajor

instance BLASEnum Transpose CblasTranspose where
  toBlas Trans          = CblasTrans
  toBlas NoTrans        = CblasNoTrans
  fromBlas CblasTrans   = Trans
  fromBlas CblasNoTrans = NoTrans

instance BLASEnum UpLo CblasUplo where
  toBlas Up           = CblasUpper
  toBlas Lo           = CblasLower
  fromBlas CblasUpper = Up
  fromBlas CblasLower = Lo


instance LAPACKEEnum Order Int where
    toLapacke e    = fromEnum (toBlas e :: CblasOrder)
    fromLapacke le = fromBlas (toEnum le :: CblasOrder)


{-| Generic matrix interface. -}
class (Field1 e, Indexable (mat e) IndexPair e) => GMatrix mat e where
  -- matrix   :: Shape -> mat e
  shape    :: mat e -> Shape
  rowCount :: mat e -> Index
  colCount :: mat e -> Index
  shape m  = (rowCount m, colCount m)
  rowCount = fst . shape
  colCount = snd . shape

  -- (!) :: mat e -> IndexPair -> e

class (Field1 e, BlasOps e, GMatrix mat e, CMatrix mat e) => MatrixMatrix mat e where
  (##) :: mat e -> mat e -> mat e
  (##!) :: (mat e, Transpose) -> (mat e, Transpose) -> mat e
  (##+) :: mat e -> mat e -> mat e
  (##-) :: mat e -> mat e -> mat e
  m1 ## m2 | colCount m1 /= rowCount m2 = error "(##): shape mismatch!"
           | otherwise = unsafePerformIO $ matrixMult 1 NoTrans m1 NoTrans m2
  (m1,t1) ##! (m2,t2) | colCountTrans t1 s1 /= rowCountTrans t2 s2 = error "(##): shape mismatch!"
                      | otherwise = unsafePerformIO $ matrixMult 1 t1 m1 t2 m2
                          where s1 = shape m1
                                s2 = shape m2
  m1 ##+ m2 = matrixBinMap (\a b -> a + b) m1 m2
  m1 ##- m2 = matrixBinMap (\a b -> a - b) m1 m2


class (CMatrix mat e, CVector vec e) => MatrixVector mat vec e where
  (#|) :: mat e -> vec e -> vec e
  (|#) :: vec e -> mat e -> vec e


{-| Matrix operations with a scalar. 
    The nomenclature is to be read /Matrix - Scalar - [operation name]/,
    where /#/ stands for matrix, /./ stands for scalar. -}
class (Storable e, CMatrix mat e) => MatrixScalar mat e where
  (#.*) :: mat e -> e -> mat e
  a #.* b = matrixMap (*b) a
  (#./) :: mat e -> e -> mat e
  a #./ b = matrixMap (/b) a
  (#.+) :: mat e -> e -> mat e
  a #.+ b = matrixMap (+b) a
  (#.-) :: mat e -> e -> mat e
  a #.- b = matrixMap ((-)b) a


{-| Interface for matrices with underlying contiguous C array storage.
    These matrices can be used with BLAS and LAPACK functions. -}
class (Storable e, BlasOps e, GMatrix mat e) => CMatrix mat e where
  -- | This is an associated vector type that /can/ be used with /mat e/.
  type CMatrixVector mat e :: *     
  -- | The same, but a vector type with a type that is the associated scalar of e. 
  type CMatrixVectorS mat e :: *    
  matrixAlloc      :: Shape -> IO (mat e)
  withCMatrix      :: mat e -> (Ptr e -> IO a) -> IO a
  lda              :: mat e -> Index
  order            :: mat e -> Order
  matrixForeignPtr :: mat e -> ForeignPtr e


{-| Map over a CMatrix. 
Applies the given function to each element in the matrix and returns the resulting matrix. -}
matrixMap :: (Storable e1, Storable e2, CMatrix mat1 e1, CMatrix mat2 e2) => 
             (e1 -> e2)  -- ^ Function /f/ to map.
             -> mat1 e1  -- ^ Input matrix A.
             -> mat2 e2  -- ^ Return matrix B. /B_ij = f A_ij/.
matrixMap f mat = unsafePerformIO $ let s = shape mat in 
                  matrixAlloc s >>= \m -> unsafeMatrixMap f mat m >> return m

{-| Map a binary function over two 'CMatrix's. -}
matrixBinMap :: (Storable e1, Storable e2, Storable e3, CMatrix mat1 e1, CMatrix mat2 e2, CMatrix mat3 e3) => 
                (e1 -> e2 -> e3) -- ^ Function /f/ to map.
                -> mat1 e1      -- ^ First input matrix A.
                -> mat2 e2      -- ^ Second input matrix B.
                -> mat3 e3      -- ^ Return matrix C. /C_ij = f A_ij B_ij/.
matrixBinMap f mat1 mat2 = unsafePerformIO $ do
                             let (m1,n1) = shape mat1
                                 (m2,n2) = shape mat2
                             m <- matrixAlloc (min m1 m2, min n1 n2) 
                             unsafeMatrixBinMap f mat1 mat2 m
                             return m
                                               



data CMatrixContainer = forall mat a. CMatrix mat a => CMatrixContainer (mat a)

-- This function finds the quadruples (a,b,c,d) for each matrix,
-- saying that: go through /a/ elements using /b/ as increment, then
-- increment the pointer (from the start of the line) using increment /d/,
-- and do that /c/ times. Naturally, (a,b) should be equal for all matrices
-- if they have the same shape.
-- If more of the matrices are RowMajor than ColumnMajor, the returned 
-- iteration order will be row-wise, otherwise it will be column-wise.
-- These functions are used in unsafeMatrixMap and friends.
lengthAndInc' :: [CMatrixContainer] -> [(Index, Index, Index, Index)]
lengthAndInc' mas = if nr > nc then as else bs
    where
      as = map lengthAndInc'' mas
      lengthAndInc'' (CMatrixContainer a) = lengthAndInc a
      bs = map flipit as
      flipit (a,b,c,d) = (b,a,d,c)
      (rm,cm) = partition (== RowMajor) os
      (nr,nc) = (length rm, length cm)
      os = map (\(CMatrixContainer m) -> order m) mas
lengthAndInc :: forall mat a. (CMatrix mat a) => mat a -> (Index, Index, Index, Index)
lengthAndInc ma = case o of
                    RowMajor -> (n,m,1,ldA)
                    _ -> (n,m,ldA,1)
    where o = order ma
          (m,n) = shape ma
          ldA = lda ma


unsafeMatrixMap :: (Storable e1, Storable e2, CMatrix mat1 e1, CMatrix mat2 e2) => (e1 -> e2) -> mat1 e1 -> mat2 e2 -> IO ()
unsafeMatrixMap f mat mat' = 
    let 
        [(n1,m1,i11,i12),(n2,m2,i21,i22)] = lengthAndInc' [CMatrixContainer mat, CMatrixContainer mat']
   in
    withCMatrix mat $ \matp -> do
      withCMatrix mat' $ \mat'p ->
          unsafePtrMapInc2 (i11,i12) (i21,i22) f matp mat'p ((min n1 n2),(min m1 m2))


unsafeMatrixBinMap :: (Storable e1, Storable e2, Storable e3, CMatrix mat1 e1, CMatrix mat2 e2, CMatrix mat3 e3) => (e1 -> e2 -> e3) -> mat1 e1 -> mat2 e2 -> mat3 e3 -> IO ()
unsafeMatrixBinMap f mat mat' mat'' = 
    let 
        [(n1,m1,i11,i12),(n2,m2,i21,i22),(n3,m3,i31,i32)] = lengthAndInc' [CMatrixContainer mat, CMatrixContainer mat', CMatrixContainer mat'']
   in
    withCMatrix mat $ \matp ->
    withCMatrix mat' $ \mat'p ->
    withCMatrix mat'' $ \mat''p ->
          unsafe2PtrMapInc2 (i11,i12) (i21,i22) (i31,i32) f matp mat'p mat''p ((minimum [n1,n2,n3]),(minimum [m1,m2,m3]))



-- data (Storable a) => BlasComplex a = BlasComplex { bcReal :: a, bcImag :: a }


{-| This is the instance of 'CMatrix' that Jalla provides.
    If you don't have another 'CMatrix' instance, 'Matrix'
    is the one you will want to use. -}
data BlasOps e => Matrix e = Matrix { matP :: !(ForeignPtr e),
                                     matShape :: !Shape,
                                     matLDA :: !Index,
                                     matOrder :: !Order }


instance (Num e, Field1 e, BlasOps e) => GMatrix Matrix e where
  -- matrix (r,c) = unsafePerformIO $ matrixAlloc (r,c) >>= \m -> unsafeMatrixFill m 42 >> return m
  shape = matShape
  -- m ! ij = unsafePerformIO $ matrixElem m ij

instance BlasOps e => MatrixMatrix Matrix e 

instance BlasOps e => Indexable (Matrix e) IndexPair e where
    m ! ij = unsafePerformIO $ matrixElem m ij


instance (Num e, Field1 e, BlasOps e) => CMatrix Matrix e where
  type CMatrixVector Matrix e  = Vector e
  type CMatrixVectorS Matrix e = Vector (FieldScalar e)
  matrixAlloc                  = matrixAlloc'
  withCMatrix                  = withMatrix'
  lda                          = matLDA
  order                        = matOrder
  matrixForeignPtr             = matP


withMatrix' :: (BlasOps e) => Matrix e -> (Ptr e -> IO a) -> IO a
withMatrix' m = withForeignPtr (matP m)


instance (BlasOps e, Show e) => Show (Matrix e) where
  show mat = "listMatrix (" ++ show m ++ "," ++ show n ++ ") " ++ show ml
    where (m,n) = shape mat
          ml    = matrixList RowMajor mat


instance (BlasOps e, Eq e) => Eq (Matrix e) where
  a == b = if (shape a == shape b) 
           then (and $ zipWith (==) (matrixList RowMajor a) (matrixList RowMajor b))
           else False


{-| /Num/ instance for a /Matrix/. 
The operations are all /element-wise/. There may be the occasional error
by wrongly assuming that /(*)/ returns the matrix product, which it doesn't.
This instance is basically only provided to get the + and - operators.
Note that this will /not/ work with 'sum', since 
that assumes it can start with a "0". -}
instance (BlasOps e, Num e) => Num (Matrix e) where
  a + b         = a ##+ b
  a - b         = a ##- b
  a * b         = matrixBinMap (*) a b
  negate        = matrixMap (* (-1))
  abs           = matrixMap abs
  signum        = matrixMap signum
  fromInteger i = createMatrix (1,1) $ setElem (0,0) (fromIntegral i)
  

instance (BlasOps e, Num e, Fractional e) => Fractional (Matrix e) where
  a / b = matrixBinMap (/) a b
  recip = matrixMap recip
  fromRational r = createMatrix (1,1) $ setElem (0,0) (fromRational r)
  
{-| An instance of 'Matrix' for 'Floating', for convenience.
    Some of these don't make much sense in some situations,
    but having the trigonometric functions and the like around can be pretty handy. 
    The functions work element-wise. -}
instance (BlasOps e, Num e, Fractional e) => Floating (Matrix e) where
  -- | Returns a 1-vector with /pi/ in it.
  pi = createMatrix (1,1) $ setElem (0,0) pi
  exp = matrixMap exp
  sqrt = matrixMap sqrt
  log = matrixMap log
  -- | Takes the /element-wise/ power.
  a ** b = matrixBinMap (**) a b
  -- | Computes 'logBase' the /element-wise/. It may be more useful to simply use /matrixMap (logBase b) v/.
  logBase = matrixBinMap logBase
  sin = matrixMap sin
  tan = matrixMap tan
  cos = matrixMap cos
  asin = matrixMap asin
  atan = matrixMap atan
  acos = matrixMap acos
  sinh = matrixMap sinh
  tanh = matrixMap tanh
  cosh = matrixMap cosh
  asinh = matrixMap asinh
  atanh = matrixMap atanh
  acosh = matrixMap acosh



{-| Get association list of indices and elements for the given GMatrix. -}
gmatrixAssocs :: (GMatrix mat e) => mat e -> [(IndexPair,e)]
gmatrixAssocs m = zip is $ map (m !) is
    where
        is = range ((0,0),s)
        s = let (r,c) = shape m in (r-1,c-1)
{-# RULES "gmatrixAssocs/matrixAssocs" forall (m :: (BlasOps e, CMatrix mat e) => mat e). gmatrixAssocs m = matrixAssocs RowMajor m #-}


{-| Get association list of indices and elements for the given CMatrix. -}
matrixAssocs :: (BlasOps e, CMatrix mat e) => Order -> mat e -> [(IndexPair, e)]
matrixAssocs o mat = zip r es
    where
        r | o == RowMajor = [(i,j) | i <- [0..r'], j <- [0..c']] 
          | otherwise    = [(i,j) | j <- [0..c'], i <- [0..r']] 
        es = matrixList o mat
        (r',c') = let (a,b) = shape mat in (a-1,b-1)
{-# SPECIALIZE INLINE matrixAssocs :: Order -> Matrix CFloat -> [(IndexPair, CFloat)] #-}
{-# SPECIALIZE INLINE matrixAssocs :: Order -> Matrix CDouble -> [(IndexPair, CDouble)] #-}
{-# SPECIALIZE INLINE matrixAssocs :: Order -> Matrix (Complex CFloat) -> [(IndexPair, Complex CFloat)] #-}
{-# SPECIALIZE INLINE matrixAssocs :: Order -> Matrix (Complex CDouble) -> [(IndexPair, Complex CDouble)] #-}


{-| Matrix multiplication. Computes alpha * A(^T) * B(^T). -}
matrixMult :: (BlasOps e, CMatrix mat e) =>
    e               -- ^ Factor alpha
    -> Transpose     -- ^ Transposition of matrix A
    -> mat e         -- ^ Matrix A
    -> Transpose     -- ^ Transposition of Matrix B
    -> mat e         -- ^ Matrix B
    -> IO (mat e)
{-# SPECIALIZE INLINE matrixMult :: CFloat -> Transpose -> Matrix CFloat -> Transpose -> Matrix CFloat -> IO (Matrix CFloat) #-}
{-# SPECIALIZE INLINE matrixMult :: CDouble -> Transpose -> Matrix CDouble -> Transpose -> Matrix CDouble -> IO (Matrix CDouble) #-}
{-# SPECIALIZE INLINE matrixMult :: (Complex CFloat) -> Transpose -> Matrix (Complex CFloat) -> Transpose -> Matrix (Complex CFloat) -> IO (Matrix (Complex CFloat)) #-}
{-# SPECIALIZE INLINE matrixMult :: (Complex CDouble) -> Transpose -> Matrix (Complex CDouble) -> Transpose -> Matrix (Complex CDouble) -> IO (Matrix (Complex CDouble)) #-}
matrixMult alpha transA a transB b =
  matrixAlloc s >>= \ret ->
  unsafeMatrixMult alpha transA a transB b 0 ret >>
  return ret
    where s = (rowCountTrans transA (shape a), colCountTrans transB (shape b))


{-| Unsafe matrix multiplication. The result is accumulated in the last matrix argument; this function is unsafe
because the memory of the last argument is changed in place. This can be used for accumulating
many operations in a monad, maybe? Computes C <- alpha * A(^T) * B(^T) + beta * C -}
unsafeMatrixMult :: (BlasOps e, CMatrix mat e) =>
    e            -- ^ Factor alpha
    -> Transpose  -- ^ Transposition of matrix A
    -> mat e      -- ^ Matrix A
    -> Transpose  -- ^ Transposition of Matrix B
    -> mat e      -- ^ Matrix B
    -> e          -- ^ Factor beta
    -> mat e      -- ^ Matrix C -- This is changed in place and /must/ be of the correct size! The
                 -- size is not checked!
    -> IO ()
unsafeMatrixMult alpha transA a transB b beta c =
  withCMatrix a $ \pa ->
  withCMatrix b $ \pb ->
  withCMatrix c $ \pc ->
  gemm (toBlas $ order a) transA' transB' m n k alpha pa ldA pb ldB beta pc ldC
    where
      (m,k)   = shapeTrans transA $ shape a
      n       = colCountTrans transB $ shape b
      ldA     = lda a
      ldB     = lda b
      ldC     = lda c
      transA' = toBlas transA
      transB' = toBlas transB



{-| Solve a system AX = B with LAPACKs xgesv procedure. Replaces A with a LU decomposition and B with the solution. -}
unsafeSolveLinearSystem :: (BlasOps e, LapackeOps e se, CMatrix mat e) =>
                          mat e    -- ^ Matrix A
                          -> mat e  -- ^ Matrix B, holds the result after the method returned.
                          -> IO ()
unsafeSolveLinearSystem a b | rowCount a == colCount a && rowCount a == rowCount b =
  withCMatrix a $ \pa ->
  withCMatrix b $ \pb ->
  allocaArray n $ \pipiv ->
  gesv (fromEnum ((toBlas $ order a) :: CblasOrder)) n nrhs pa (lda a) pipiv pb (lda b) >>= \ret ->
  if ret /= 0 then error "unsafeSolveLinearSystem: ret /= 0" else return ()
    where
      n = colCount a
      nrhs = colCount b
unsafeSolveLinearSystem a b | otherwise = error "unsafeSolveLinearSystem: The shapes of the arguments do not match."


{-| Solves a system AX = B with LAPACKs xgesv procedure. Returns
    a matrix with the solutions in its columns. -}
solveLinearSystem :: (BlasOps e, LapackeOps e se, CMatrix mat e) => 
                     mat e   -- ^ The matrix /A/
                     -> mat e -- ^ The matrix /B/, the right-hand sides.
                     -> mat e -- ^ The solutions /X/, one in each column.
{-# NOINLINE solveLinearSystem #-}
solveLinearSystem a b = unsafePerformIO $
                        matrixCopy b NoTrans >>= \x ->
                        matrixCopy a NoTrans >>= \a' ->
                        unsafeSolveLinearSystem a' x >> return x


{-| Returns the square identity matrix with given row number. -}
idMatrix :: (BlasOps e, CMatrix mat e) => Index -> mat e
idMatrix n = createMatrix (n,n) $ fill 0 >> setDiag 0 (repeat 1)


{-| Invert by solving a linear system. 'invert' is probably more efficient. -}
invert' :: (BlasOps e, LapackeOps e se, CMatrix mat e) => mat e -> mat e
invert' a | colCount a == rowCount a = solveLinearSystem a (idMatrix $ colCount a)
          | otherwise = error "Cannot invert non-square matrix."


{-| Invert. Implemented with LAPACK's getrf and getri, that is probably more efficient than
using solveLinearSystem. -}
{-# NOINLINE invert #-}
invert :: (BlasOps e, LapackeOps e se, CMatrix mat e) => mat e -> Maybe (mat e)
invert a | colCount a == rowCount a = unsafePerformIO $ matrixCopy a NoTrans >>= \a' -> unsafeInvert a'
         | otherwise = Nothing --error "Cannot invert non-square matrix."


{-| Compute the pseudo-inverse with the help of a SVD. -}
pseudoInverse :: (BlasOps e, se ~ FieldScalar e, BlasOps se, Real se, LapackeOps e se, MatrixMatrix mat e, CMatrix mat e) 
                 => mat e -> mat e
pseudoInverse a = (matrixMultDiag (vt,Trans) s, NoTrans) ##! (u,Trans)
  -- ((vt,Trans) ##! (sm,NoTrans), NoTrans) ##! (u,Trans)
  where svd'  = (svd a (SVDU SVDThin, SVDVT SVDThin))
        s     = map (\x -> if x /= 0 then 1 / (realToFrac x) else 0) $ svdS svd'
        u     = fromJust $ svdU svd'
        vt    = fromJust $ svdVT svd'


{-| A construct to enable us to reference rows and columns in matrices, thereby
saving some cost on copying and memory allocation. The referenced matrix will not be
garbage collected (if I understand 'ForeignPtr' right) before one of the 'RefVector's
referencing it. -}
data Storable e => RefVector e = RefVector {
  refRefP :: !(ForeignPtr e),
  refVecP :: !(Ptr e),
  refVecInc :: !Index,
  refVecLength :: !Index}


instance (Show e, Field1 e, Storable e, BlasOps e) => Show (RefVector e) where
  show v = "listVector " ++ show (vectorList v)

instance (BlasOps e, Storable e) => CVector RefVector e where
  vectorAlloc = error "No vectorAlloc for RefVector."
  withCVector v act = act $ refVecP v
  inc         = refVecInc

instance (BlasOps e, Storable e) => Indexable (RefVector e) Index e where
  v ! i  = if i >= 0 && i < refVecLength v 
           then unsafePerformIO $ withCVector v $ \p -> peek (advancePtr p (i * (refVecInc v)))
           else error "RefVector range violation."
                       
instance (Field1 e, Storable e, BlasOps e) => GVector RefVector e where
  vector n = unsafePerformIO $ vectorAlloc n
  vectorLength = refVecLength

instance BlasOps e => VectorScalar RefVector e


{-| Run an IO action on a row of a matrix, without copying the vector. -}
withCMatrixRow :: Storable e => CMatrix mat e => mat e -> Index -> (RefVector e -> IO a) -> IO a
withCMatrixRow mat i act = withCMatrix mat $ \mp -> do
  when (i >= m || i < 0) $ error "withCMatrixRow range violation." 
  let p = advancePtr mp (i * rinc)
  act (RefVector { refRefP = (matrixForeignPtr mat), refVecP = p, refVecInc = cinc, refVecLength = n })
  where
    (m,n) = shape mat
    o = order mat
    (rinc,cinc) | o == RowMajor = (lda mat, 1)
                | otherwise = (1, lda mat)
            
{-| Run an IO action on a column of a matrix, without copying the vector. -}
withCMatrixColumn :: Storable e => CMatrix mat e => mat e -> Index -> (RefVector e -> IO a) -> IO a
withCMatrixColumn mat i act = withCMatrix mat $ \mp -> do
  when (i >= n || i < 0) $ error "withCMatrixColumn range violation." 
  let p = advancePtr mp (i * cinc)
  act (RefVector { refRefP = (matrixForeignPtr mat), refVecP = p, refVecInc = rinc, refVecLength = m })
  where
    (m,n) = shape mat
    o = order mat
    (rinc,cinc) | o == RowMajor = (lda mat, 1)
                | otherwise = (1, lda mat)
            

{-| Return a 'RefVector' to a column or row. -}
columnRef, rowRef :: (CMatrix mat e) => mat e -> Index -> RefVector e
{-# NOINLINE columnRef #-}
columnRef m i = unsafePerformIO $ withCMatrixColumn m i return
{-# NOINLINE rowRef #-}
rowRef m i = unsafePerformIO $ withCMatrixRow m i return


{-| Return 'RefVector's to all rows or columns. -}
rowsRef, columnsRef :: (CMatrix mat e) => mat e -> [RefVector e]
rowsRef m = map (rowRef m) [0..(rowCount m)-1]
columnsRef m = map (columnRef m) [0..(colCount m)-1]


-- Note: copyVector can not work with /RefVector/, since those can not be allocated.
{-| Get a column or row from a matrix. -}
column, row :: (CMatrix mat e, CVector vec e) => mat e -> Index -> vec e
row m i = unsafePerformIO $ withCMatrixRow m i $ \ref -> copyVector ref -- A copy should be safe.
column m i = unsafePerformIO $ withCMatrixColumn m i $ \ref -> copyVector ref 
{-# SPECIALIZE NOINLINE row :: Matrix CFloat -> Index -> Vector CFloat #-}  
{-# SPECIALIZE NOINLINE row :: Matrix CDouble -> Index -> Vector CDouble #-}  
{-# SPECIALIZE NOINLINE row :: Matrix (Complex CFloat) -> Index -> Vector (Complex CFloat) #-}  
{-# SPECIALIZE NOINLINE row :: Matrix (Complex CDouble) -> Index -> Vector (Complex CDouble) #-}  
{-# SPECIALIZE NOINLINE column :: Matrix CFloat -> Index -> Vector CFloat #-}  
{-# SPECIALIZE NOINLINE column :: Matrix CDouble -> Index -> Vector CDouble #-}  
{-# SPECIALIZE NOINLINE column :: Matrix (Complex CFloat) -> Index -> Vector (Complex CFloat) #-}  
{-# SPECIALIZE NOINLINE column :: Matrix (Complex CDouble) -> Index -> Vector (Complex CDouble) #-}  
{-# RULES "row/rowRef" row = rowRef #-}
{-# RULES "colum/columnRef" column = columnRef #-}

{-| Get all rows or columns from a matrix. -}
rows, columns :: (CMatrix mat e, CVector vec e) => mat e -> [vec e]
rows m = map (row m) [0..(rowCount m) - 1]
columns m = map (column m) [0..(colCount m) - 1]
{-# SPECIALIZE NOINLINE rows :: Matrix CFloat -> [Vector CFloat] #-}  
{-# SPECIALIZE NOINLINE rows :: Matrix CDouble -> [Vector CDouble] #-}  
{-# SPECIALIZE NOINLINE rows :: Matrix (Complex CFloat) -> [Vector (Complex CFloat)] #-}  
{-# SPECIALIZE NOINLINE rows :: Matrix (Complex CDouble) -> [Vector (Complex CDouble)] #-}  
{-# SPECIALIZE NOINLINE columns :: Matrix CFloat -> [Vector CFloat] #-}  
{-# SPECIALIZE NOINLINE columns :: Matrix CDouble -> [Vector CDouble] #-}  
{-# SPECIALIZE NOINLINE columns :: Matrix (Complex CFloat) -> [Vector (Complex CFloat)] #-}  
{-# SPECIALIZE NOINLINE columns :: Matrix (Complex CDouble) -> [Vector (Complex CDouble)] #-}  
{-# RULES "rows/rowsRef" rows = rowsRef #-}
{-# RULES "colums/columnsRef" columns = columnsRef #-}


{-| Multiply a matrix with a diagonal matrix, given only as a list of diagonal entries.
This uses references instead of copied vectors, to work more memory efficient
with large matrices. -}
matrixMultDiag :: (BlasOps e) => CMatrix mat e => 
                  (mat e, Transpose) -- ^ Matrix /A/ and information on whether to use it transposed or not.
                  -> [e]              -- ^ Diagonals of a matrix /S/
                  -> mat e            -- ^ /A * S/ or /A^T * S/.
matrixMultDiag (a,t) d = modifyMatrix a t $ zipWithM_ scaleColumn d [0..c-1]
  where sh@(_,c) = shapeTrans t (shape a)
--I really want to work in-place, and I should be able to in the MMM monad, in a safe way 
--(only the matrix under construction can be modified).



{-| SVD option for the /U/ output. -}
data SVDU = SVDU SVDOpt deriving (Ord, Eq)
{-| SVD option for the /VT/ output. -}
data SVDVT = SVDVT SVDOpt deriving (Ord, Eq)
{-| SVD options for the output. -}
data SVDOpt = SVDFull -- ^ Selects the output to be fully computed. For /U/, that means /m x m/, for /VT/ it means /n x n/.
            | SVDThin -- ^ Selects Thin SVD. /U/: (m, min (m,n)), /VT/: (n, min (m,n))
            | SVDNone -- ^ Deselects the output.
            deriving (Ord, Eq)
  

svdJob :: SVDOpt -> CChar
svdJob SVDFull = toEnum $ fromEnum 'A'
svdJob SVDThin = toEnum $ fromEnum 'S'
svdJob SVDNone = toEnum $ fromEnum 'N'
                                        

svdJobs :: (SVDU, SVDVT) -> (CChar,CChar)
svdJobs (SVDU u,SVDVT vt) = (svdJob u, svdJob vt)


{-| Description of the result of a singular value decomposition with 'svd'. -}
data (CMatrix mat e) => SVD mat e = SVD { 
  -- | The left, unitary matrix U. Nothing if the /SVDU SVDNone/ was selected.
  svdU :: Maybe (mat e)
  -- | The right singular vectors, VT (transposed, so the vectors are in the rows). Nothing if /SVDVT SVDNone/ was selected.
  , svdVT :: Maybe (mat e)
  -- | The singular values, /s/.  
  , svdS :: [FieldScalar e] }
                                                            

-- s /must/ have increment 1!!
{-| Uses the LAPACKE function /gesvd/ internally to compute the singular value decomposition. 
    The arguments are used as storage, so this is really unsafe. Only used internally. -}
unsafeSVD :: (BlasOps e, LapackeOps e se, CVector vec se, CMatrix mat e) => 
             mat e            -- ^ The matrix to diagonalise.
             -> (SVDU, SVDVT)  -- ^ Options for the SVD.
             -> vec se         -- ^ The CVector for holding the singular values.
             -> mat e          -- ^ U
             -> mat e          -- ^ VT
             -> IO Int         -- ^ The return value of gesvd.
unsafeSVD a opts s u vt = do
  when (inc s /= 1) $ error $ "unsafeSVD: s must have increment 1, but has " ++ show (inc s)
  withCMatrix a $ \ap ->
    withCVector s $ \sp ->
    withCMatrix u $ \up ->
    withCMatrix vt $ \vtp ->
    mallocForeignPtrArray superb_size >>= \superb' -> withForeignPtr superb' $ \superbp -> do
      gesvd mOrder jobu jobvt m n ap (lda a) sp up (lda u) vtp (lda vt) superbp
  where (jobu, jobvt) = svdJobs opts
        mOrder = toLapacke $ order a
        (m,n) = shape a
        superb_size = (min m n) - 1 -- This size is taken from the LAPACKE source code.

{-| Compute the singular value decomposition /U * S * V^T = A/ of a matrix /A/.
    U and V are (m,m) and (n,n) unitary matrices, and S is a (m,n) matrix with
    nonzero elements only on the main diagonal. These are the /singular values/.
    
    The extent to which /U/ and /V^T/
    are computed can be chosen by 'SVDU' and 'SVDVT' arguments.
    SVDU or SVDVT 'SVDFull' return the full (m,m) or (n,n) matrices.
    For 'SVDU' 'SVDThin', only the first min(m,n) columns of /U/ are computed.
    For 'SVDVT' 'SVDThin', only the first min(m,n) rows of /V^T/ are computed.
    For 'SVDNone', the respective matrix will not be returned.

    Note that /V^T/ is indeed returned in its transposed form. -}
svd :: (BlasOps e, se ~ FieldScalar e, BlasOps se, LapackeOps e se, CMatrix mat e) =>
       mat e               -- ^ The matrix /A/
       -> (SVDU, SVDVT)     -- ^ Choice of extent to which to compute /U/ and /V^T/.
       -> SVD mat e  -- ^ Returns the SVD.
svd a opts@(SVDU optu, SVDVT optvt) =
  unsafePerformIO $ do
    matrixCopy a NoTrans >>= \acopy ->
      matrixAlloc (shapeU optu) >>= \u ->
      matrixAlloc (shapeVT optvt) >>= \vt ->
      vectorAlloc len_s >>= \(s :: Vector se) -> do
        unsafeSVD acopy opts s u vt
        return $ SVD { svdU = if optu /= SVDNone then Just u else Nothing
                     , svdVT = if optvt /= SVDNone then Just vt else Nothing
                     , svdS = vectorList s }
  where
    (m,n) = shape a
    len_s = min m n
    shapeU SVDFull = (m,m)
    shapeU SVDThin = (m, min m n)
    shapeU _ = (0,0)
    shapeVT SVDFull = (n,n)
    shapeVT SVDThin = (min m n, n)
    shapeVT _ = (0,0)


unsafeInvert :: (BlasOps e, LapackeOps e se, CMatrix mat e) => mat e -> IO (Maybe (mat e))
unsafeInvert mat = withCMatrix mat $ \mp ->
                   allocaArray (min m n) $ \ipiv ->
                   getrf o m n mp ldA ipiv >>= \ret ->
                   if ret /= 0
                   then return Nothing
                   else getri o n mp ldA ipiv >>= \ret ->
                        if ret /= 0
                        then return Nothing
                        else return $ Just mat
                   where
                     o = toLapacke $ order mat
                     ldA = lda mat
                     (m,n) = shape mat


matrixAlloc' :: (BlasOps e) => Shape -> IO (Matrix e)
matrixAlloc' (r,c) = mallocForeignPtrArray (r * c) >>=
                     \p -> return $ Matrix p (r,c) c RowMajor


checkIndex :: Shape -> IndexPair -> Bool
checkIndex (r,c) (i,j) = inRange (0,r-1) i && inRange (0,c-1) j


inMatrixRange :: (BlasOps e, GMatrix mat e) => mat e -> IndexPair -> Bool
inMatrixRange m i = checkIndex (shape m) i


matrixElem :: (Num e, BlasOps e, CMatrix mat e) => mat e -> IndexPair -> IO e
matrixElem m (i,j) | not (checkIndex s (i,j)) = error $ "matrixElem out of bounds"
  where s = shape m
matrixElem m (i,j) | otherwise = withCMatrix m $
                                 \p -> peekElemOff p (i' * (lda m) + j')
                                   where (i',j') | order m == RowMajor = (i,j)
                                                 | otherwise = (j,i)


{-| Sets an element in place, therefore this is unsafe. Range check is done,
and an error is raised if the given index is out of bounds. -}
unsafeMatrixSetElem :: (BlasOps e, CMatrix mat e) =>
                      mat e  -- ^ Matrix to be modified.
                      -> IndexPair  -- ^ Index of the element to set
                      -> e         -- ^ Element to set
                      -> IO ()
unsafeMatrixSetElem mat (i,j) he | not (checkIndex s (i,j)) = error $ "unsafeMatrixSetElem out of bounds"
  where s = shape mat
unsafeMatrixSetElem mat (i,j) he | otherwise = withCMatrix mat $
                                               \p -> pokeElemOff p (i' * (lda mat) + j') he
                                                 where (i',j') | order mat == RowMajor = (i,j)
                                                               | otherwise = (j,i)


{-| Setting a bunch of elements in a CMatrix; more efficient than calling unsafeMatrixSetElem repeatedly. -}
unsafeMatrixSetElems :: (BlasOps e, CMatrix mat e) =>
                      mat e  -- ^ Matrix to be modified.
                      -> [(IndexPair, e)]  -- ^ List of indices of the elements to set, together with the elements.
                      -> IO ()
-- unsafeMatrixSetElems mat els | not (checkIndex s (i,j)) = error $ "unsafeMatrixSetElem out of bounds"
--  where s = shape mat
unsafeMatrixSetElems mat els = withCMatrix mat $
                               \p -> mapM_ (setter p) els
                                 where
                                   ld = lda mat
                                   setter' p ((i,j),e) = pokeElemOff p (i * ld + j) e
                                   setter p ((i,j),e) | order mat == RowMajor = setter' p ((i,j),e)
                                                      | otherwise = setter' p ((j,i),e)


{-| Fill the matrix in place, therefore this is unsafe. -}
unsafeMatrixFill :: (Num e, BlasOps e, CMatrix mat e) =>
                   mat e -- ^ Matrix to fill.
                   -> e        -- ^ Value to fill with
                   -> IO ()
unsafeMatrixFill m e = let (r,c) = shape m
                           f p n | n > 0 = let p' = advancePtr p 1
                                               n' = n - 1
                                           in poke p e >> f p' n'
                                 | otherwise = return ()
                        in withCMatrix m (\p' -> f p' (r * c))



{-| Copies a matrix into the storage of another matrix, in-place and therefore unsafe
Uses the BLAS copy routine from BlasOps. /NOTE/: This uses BLAS 'copy', i.e. the LDA /must/ be either m or n,
where (m,n) is the shape of the matrix. -}
unsafeMatrixCopy :: (BlasOps e, CMatrix mat e) => 
                    mat e        -- ^ Source /A/ to copy from.
                    -> Transpose  -- ^ If /Trans/, copies /A^T/ to B, otherwise copies /A/.
                    -> mat e      -- ^ Destination /B/. Must have the right dimensions.
                    -> IO () 
unsafeMatrixCopy src t dst | shapeTrans t (shape src) == shape dst = 
  case t of
    NoTrans -> zipWithM_ unsafeCopyVector src_rows dst_rows
    -- withCMatrix src $ \s ->
    -- withCMatrix dst $ \d -> copy n s 1 d 1
    Trans   -> zipWithM_ unsafeCopyVector src_cols dst_rows
  where 
    src_cols = columnsRef src
    src_rows = rowsRef src
    dst_rows = rowsRef dst
    n        = (rowCount src) * (colCount src)
unsafeMatrixCopy _ _ _ | otherwise = error "unsafeMatrixCopy: shape mismatch."


{-| Copies a matrix into a new matrix of the same shape.
When using unsafe operations which work in-place, this should be used to copy a matrix
before using such an unsafe function. -}
matrixCopy :: (BlasOps e, CMatrix mat e) => mat e -> Transpose -> IO (mat e)
matrixCopy a t = matrixAlloc (shapeTrans t (shape a)) >>= \ret -> unsafeMatrixCopy a t ret >> return ret


matrixMap' :: (BlasOps e1, BlasOps e2, CMatrix mat1 e1, CMatrix mat2 e2) => (e1 -> e2) -> mat1 e1 -> IO (mat2 e2)
matrixMap' f mat = matrixAlloc s >>= \mRet ->
  withCMatrix mat $ \p1 ->
  withCMatrix mRet $ \p2 ->
  unsafePtrMap f p1 p2 n >> return mRet
  where
    s@(r,c) = shape mat
    n = r * c



{-| Create a list of elements, in row-major order, from the given matrix. -}
matrixList :: (GMatrix mat e) => Order -> mat e -> [e]
matrixList o mat | o == RowMajor = [mat ! (i,j) | i <- [0..(r-1)], j <- [0..(c-1)]]
                 | o == ColumnMajor = [mat ! (i,j) | j <- [0..(c-1)], i <- [0..(r-1)]]
  where (r,c) = shape mat
{-# SPECIALIZE INLINE matrixList :: Order -> Matrix CFloat -> [CFloat] #-}
{-# SPECIALIZE INLINE matrixList :: Order -> Matrix CDouble -> [CDouble] #-}
{-# SPECIALIZE INLINE matrixList :: Order -> Matrix (Complex CFloat) -> [Complex CFloat] #-}
{-# SPECIALIZE INLINE matrixList :: Order -> Matrix (Complex CDouble) -> [Complex CDouble] #-}



{-| Create a list of lists of elements from a matrix, representing the rows of the matrix. -}
matrixLists :: (GMatrix mat e) => mat e -> [[e]]
matrixLists mat = let (r,c) = shape mat
                  in [[mat ! (i,j) | j <- [0..(c-1)]] | i <- [0..(r-1)]]


{-| Create a matrix from a list of elements, stored in row-major. -}
listMatrix :: (BlasOps e, CMatrix mat e) =>
           Shape -- ^ Shape of the matrix
           -> [e] -- ^ List of elements, row-major order
           -> mat e -- ^ If the number of elements in the list matches the number needed for the given shape exactly, returns a Just Matrix; otherwise, returns Nothing.
listMatrix (r,c) l = if c < 0 || c < 0
                     then error "Negative matrix shape??"
                     else createMatrix (r,c) $ setElems' $ zip [(i,j) | i <- [0..(r-1)], j <- [0..(c-1)]] l
                            -- mapM (uncurry setElem) $ zip [(i,j) | i <- [0..(r-1)], j <- [0..(c-1)]] l


prettyPrintMatrix :: (GMatrix mat e) => mat e -> [String]
prettyPrintMatrix m = map ppl $ matrixLists m
  where
    pp a = show a ++ " "
    ppl = concatMap pp

prettyPrintMatrixIO :: (GMatrix mat e) => mat e -> IO ()
prettyPrintMatrixIO m = mapM_ putStrLn $ prettyPrintMatrix m

----------------------------------
-- Monadic matrix manipulation
-- This type is not exported.
type MMonad mat e = StateT (mat e) IO

{-| Matrix modification monad. This is used for creating and modifying matrices efficiently. -}
newtype (BlasOps e, CMatrix mat e) => MMM s mat e a = MMM { unMMM :: MMonad mat e a } deriving (Monad, Functor)


-- Make a copy of the matrix, put it in the state, and let modification functions run on it.
-- Does /not/ allow anything else to be modified than the copy of the matrix that is given as argument.
runMMM :: (BlasOps e, CMatrix mat e) => mat e -> MMM s mat e a -> IO (mat e)
runMMM mat m = matrixAlloc s >>= \ret -> unsafeMatrixCopy mat NoTrans ret >> execStateT (unMMM m) ret
  where s = shape mat


instance (BlasOps e, CMatrix mat e) => IMM (MMM s mat e) IndexPair (mat e) e where
    setElem  = setElem'
    setElems = setElems'
    fill     = fill'
    getElem  = getElem'



{-| Create a new matrix of given size and run the given modification action on it; then return
    The new matrix. -}
createMatrix :: (BlasOps e, CMatrix mat e) =>
               Shape -- ^ (Rows, Columns)
               -> MMM s mat e a -- ^ Modification action
               -> mat e -- ^ Return value: The newly created matrix.
createMatrix s m = unsafePerformIO $ matrixAlloc s >>= execStateT (unMMM m)


{-| Scales the values in the given vector with a factor, /in place/.
    Anything else is unchanged. Uses the BLAS function 'scal'. -}
unsafeScale :: (BlasOps e, CVector vec e) => e -> vec e -> IO ()
unsafeScale alpha x = withCVector x $ \xp -> scal n alpha xp incx
  where n = vectorLength x
        incx = inc x
{-# NOINLINE unsafeScale #-}


{-| /unsafeAccum alpha x y/ computes y <- y + alpha * x, storing the result in 
/y/, therefore /destroying the old values of y/. Simply calls unsafeVectorAdd. -}
unsafeAccum :: (BlasOps e, CVector vec e) => e -> vec e -> vec e -> IO ()
unsafeAccum = unsafeVectorAdd


{-| Modify the given matrix using the given modification action; return the modified matrix. -}
modifyMatrix :: (BlasOps e, CMatrix mat e) => mat e -> Transpose -> MMM s mat e a -> mat e
modifyMatrix mat t m = unsafePerformIO $ matrixAlloc s >>= \ret ->
  unsafeMatrixCopy mat t ret >> execStateT (unMMM m) ret
    where s = shapeTrans t (shape mat)
{-# NOINLINE modifyMatrix #-}


getMatrix :: (BlasOps e, CMatrix mat e) => MMM s mat e (mat e)
getMatrix = MMM get

{-| Reference a row in the matrix under construction. See also 'row'. -}
refRow   :: CMatrix mat e => Index -> MMM s mat e (RefVector e)
refRow i = MMM $ get >>= \m -> liftIO (withCMatrixRow m i return)

{-| Reference a column in the matrix under construction. See also 'column'. -}
refColumn   :: CMatrix mat e => Index -> MMM s mat e (RefVector e)
refColumn i = MMM $ get >>= \m -> liftIO (withCMatrixColumn m i return)

{-| Scales (multiplies) the given row of the matrix under construction by a scalar. -}
scaleRow :: CMatrix mat e => 
            e                -- ^ Number to scale with.
            -> Index          -- ^ Row index. If out of range, an exception is raised.
            -> MMM s mat e ()
scaleRow alpha i = MMM $ get >>= \m -> do 
  liftIO $ withCMatrixRow m i (unsafeScale alpha)

{-| Scales (multiplies) the given column of the matrix under construction by a scalar. -}
scaleColumn :: CMatrix mat e => e -> Index -> MMM s mat e ()
scaleColumn alpha i = MMM $ get >>= \m -> do 
  liftIO $ withCMatrixColumn m i (unsafeScale alpha)


{-| Modification action: Set the value of the given element. Returns True on success, or False if the element is out of bounds. -}
setElem' :: (BlasOps e, CMatrix mat e) => IndexPair -> e -> MMM s mat e ()
setElem' (i,j) a = MMM $ get >>= \m -> liftIO (unsafeMatrixSetElem m (i,j) a)


{-| Fills the matrix that is currently under modification with a given value. -}
fill' :: (BlasOps e, CMatrix mat e) => e -> MMM s mat e ()
fill' a = MMM $ get >>= \m -> liftIO $ unsafeMatrixFill m a


{-| Sets the diagonal with given index to the given values. Operates on the matrix that is currently under modification. -}
setDiag :: (BlasOps e, CMatrix mat e) =>
          Index -- ^ Number of the diagonal. 0 Means the main diagonal, negative values mean lower diagonals, positive values mean upper diagonals.
          -> [e] -- ^ The values of the diagonal. Only as many values as fit in the diagonal are used.
          -> MMM s mat e ()  -- ^ Returns the action that sets the diagonal.
setDiag d as = MMM $ get >>= \m ->
  let (r,c) = shape m
      idxs  = diagIndices (r,c) d
  in
   case idxs of
     [] -> return ()
     ijs -> setDiag' m ijs as
     where
       setDiag' :: (BlasOps e, CMatrix mat e) => mat e -> [IndexPair] -> [e] -> MMonad mat e ()
       setDiag' m ijs as = liftIO $ unsafeMatrixSetElems m $ zip ijs as -- mapM_ (\(ij,e) -> liftIO $ unsafeMatrixSetElem m ij e) (zip ijs as)


{-| Sets elements in a matrix; caution: invalid indices are silently ommitted. -}
setElems' :: (BlasOps e, CMatrix mat e) => [(IndexPair,e)] -> MMM s mat e ()
setElems' els = MMM $ get >>= \m -> liftIO $ unsafeMatrixSetElems m els -- mapM_ (uncurry setElem)


{-| Set a row in the current matrix to a list of elements. -}
setRow :: (BlasOps e, CMatrix mat e) =>
         Index -- ^ Number of the row to set
         -> [e] -- ^ List of elements to set
         -> MMM s mat e ()
setRow i as = fmap shape getMatrix >>= \(_,c) -> setElems $ zip (range ((i,0),(i,c-1))) as -- (zip (zip [i,i..] [0..(c-1)]) as)


{-| Set a column in the current matrix to a list of elements. -}
setColumn :: (BlasOps e, CMatrix mat e) =>
            Index   -- ^ Number of the column to set
            -> [e]  -- ^ List of elements to set
            -> MMM s mat e ()
setColumn i as = fmap shape getMatrix >>= \(r,_) -> setElems $ zip (range ((0,i),(r-1,i))) as -- (zip (zip [0..(r-1)] [i,i..]) as)


{-| Set the block starting at a given index to the given CMatrix. -}
setBlock :: (BlasOps e, CMatrix mat e) =>
    IndexPair -- ^ Position in the current matrix where to put the block
    -> mat e   -- ^ Matrix to put at the given position
    -> MMM s mat e ()
setBlock (i,j) mat = getMatrix >>= \m -> setElems (a m)
    where
        a m  = as m
        is'' = range ((0,0),(r,c))
        is'  = map (\(a,b) -> (a+i,b+j)) is''
        es   = matrixList RowMajor mat
        as m = filter (\(ij,_) -> inRange ((0,0),s) ij) (zip is' es)
            where
                s = let (r,c) = shape m in (r-1,c-1)
        (r,c) = let (a,b) = shape mat in (a-1,b-1)


{-| Fill a range with a given element. -}
fillBlock :: (BlasOps e, CMatrix mat e) =>
    IndexPair    -- ^ Start of the range.
    -> IndexPair  -- ^ End of the range (inclusive).
    -> e          -- ^ Element to fill the range with.
    -> MMM s mat e ()
fillBlock start end = setElems . zip (range (start,end)) . repeat


{-| Get an element of the matrix currently under modification. -}
getElem' :: (BlasOps e, CMatrix mat e) => IndexPair -> MMM s mat e e
getElem' ij = MMM $ get >>= \m -> liftIO $ matrixElem m ij
