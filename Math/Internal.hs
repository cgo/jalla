module Math.Internal where

import Foreign
import Foreign.Ptr
import Foreign.Marshal.Array
-- import Data.Convertible

{-| Maps a function over a contiguous (C) array, storing the result in another C array (in place!),
therefore being unsafe. Internal use only. -}
unsafePtrMap :: (Storable e1, Storable e2, Integral i) =>
               (e1 -> e2) -- ^ Function to map over the C array
               -> Ptr e1   -- ^ Pointer to the array to map over
               -> Ptr e2   -- ^ Pointer to the destination array
               -> i       -- ^ Number of elements to process
               -> IO ()
unsafePtrMap = unsafePtrMapInc 1 1


{-| Maps a function over a contiguous (C) array, storing the result in the same array. -}
unsafePtrMap1 :: (Storable e, Integral i) =>
                (e -> e)    -- ^ Function to map over the C array
                -> Ptr e    -- ^ Pointer to the array to map over
                -> i        -- ^ Number of elements to process
                -> IO ()
unsafePtrMap1 = unsafePtrMap1Inc 1


{-| Maps a function over a contiguous (C) array, storing the result in another C array (in place!),
therefore being unsafe. Internal use only. -}
unsafePtrMapInc :: (Storable e1, Storable e2, Integral i) =>
               Int           -- ^ Increment (in elements) between elements in source array
               -> Int         -- ^ The same for the target array
               -> (e1 -> e2) -- ^ Function to map over the C array
               -> Ptr e1    -- ^ Pointer to the array to map over
               -> Ptr e2    -- ^ Pointer to the destination array
               -> i         -- ^ Number of elements to process
               -> IO ()
unsafePtrMapInc i1 i2 f p1 p2 n | n > 0 = fmap f (peek p1) >>= poke p2 >> unsafePtrMap f (advancePtr p1 i1) (advancePtr p2 i2) (n-1)
                                | otherwise = return ()

{-| Maps a function over a contiguous (C) array, storing the result in the same array. -}
unsafePtrMap1Inc :: (Storable e, Integral i) =>
                Int         -- ^ Increment (in elements) between elements in the array
                -> (e -> e)  -- ^ Function to map over the C array
                -> Ptr e    -- ^ Pointer to the array to map over
                -> i        -- ^ Number of elements to process
                -> IO ()
unsafePtrMap1Inc i f p n | n > 0 = pp `seq` n' `seq` fmap f (peek p) >>= poke p >> unsafePtrMap1Inc i f pp n'
  where pp = advancePtr p i
        n' = n - 1
unsafePtrMap1Inc _ _ _ _| otherwise = return ()
