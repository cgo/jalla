-----------------------------------------------------------------------------
--
-- Module      :  Math.Internal
-- Copyright   :  2011 by Christian Gosch
-- License     :  BSD3
--
-- Maintainer  : Christian Gosch <werbung@goschs.de>
-- Stability   : Experimental
-- Portability : GHC only
--
-- | This module contains functions for /internal use/ only.
--   They all base on the manipulation of pointers, using functions from the /Foreign/
--   module. They must be considered unsafe. Use only if you know what you are doing.
-----------------------------------------------------------------------------


module Jalla.Internal where

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
unsafePtrMapInc i1 i2 f p1 p2 n | n > 0 = p1' `seq` p2' `seq` n' `seq` fmap f (peek p1) >>= poke p2 >> unsafePtrMapInc i1 i2 f p1' p2' n'
                                where p1' = advancePtr p1 i1
                                      p2' = advancePtr p2 i2
                                      n'  = n - 1 
unsafePtrMapInc _ _ _ _ _ _     | otherwise = return ()


{-| Like /unsafePtrMapInc/, but for 2-dimensional C-like arrays.
This function honours the fact that in some arrays, the lines may not be contiguous. -}
unsafePtrMapInc2 :: (Storable e1, Storable e2, Integral n, Integral m) =>
               (Int,Int)   -- ^ Increment (in elements) between elements *and* lines in source array
               -> (Int,Int) -- ^ The same for the target array
               -> (e1 -> e2) -- ^ Function to map over the C array
               -> Ptr e1    -- ^ Pointer to the array to map over
               -> Ptr e2    -- ^ Pointer to the destination array
               -> (n,m)     -- ^ Number of elements to process for each line, and number of lines to process.
               -> IO ()
unsafePtrMapInc2 a@(i11,i12) b@(i21,i22) f p1 p2 (n,m) | m > 0 = unsafePtrMapInc i11 i21 f p1 p2 n >> 
                                                                 (p1' `seq` p2' `seq` m' `seq` unsafePtrMapInc2 a b f p1' p2' (n,m'))
                                                       where p1' = advancePtr p1 i12
                                                             p2' = advancePtr p2 i22
                                                             m'  = m - 1
unsafePtrMapInc2 _ _ _ _ _ _                           | otherwise = return ()




{-| Maps a function over a contiguous (C) array, storing the result in another C array (in place!),
therefore being unsafe. Internal use only. -}
unsafe2PtrMapInc :: (Storable e1, Storable e2, Storable e3, Integral i) =>
               Int           -- ^ Increment (in elements) between elements in the first input array
               -> Int         -- ^ The same for the second input array
               -> Int         -- ^ The same for the destination array
               -> (e1 -> e2 -> e3) -- ^ Function to map over the C arrays
               -> Ptr e1    -- ^ Pointer to the first input array to map over
               -> Ptr e2    -- ^ Pointer to the second source array
               -> Ptr e3    -- ^ Pointer to the destination array
               -> i         -- ^ Number of elements to process
               -> IO ()
unsafe2PtrMapInc i1 i2 i3 f p1 p2 p3 n | n > 0 = p1' `seq` p2' `seq` p3' `seq` n' `seq` do
                                                   v1 <- peek p1
                                                   v2 <- peek p2 
                                                   poke p3 (f v1 v2)
                                                   unsafe2PtrMapInc i1 i2 i3 f p1' p2' p3' n'
                                where p1' = advancePtr p1 i1
                                      p2' = advancePtr p2 i2
                                      p3' = advancePtr p3 i3
                                      n'  = n - 1 
unsafe2PtrMapInc _ _ _ _ _ _ _ _  | otherwise = return ()


{- Jetzt noch binaere Operationen auf Matrizen ... -}
unsafe2PtrMapInc2 :: (Storable e1, Storable e2, Storable e3, Integral n, Integral m) =>
               (Int,Int)   -- ^ Increment (in elements) between elements *and* lines in the first input array
               -> (Int,Int) -- ^ The same for the second input array
               -> (Int,Int) -- ^ The same for the destination array
               -> (e1 -> e2 -> e3) -- ^ Function to map over the C array
               -> Ptr e1    -- ^ Pointer to the array to map over
               -> Ptr e2    -- ^ Pointer to the second input array
               -> Ptr e3    -- ^ Pointer to the destination array
               -> (n,m)     -- ^ Number of elements to process for each line, and number of lines to process.
               -> IO ()
unsafe2PtrMapInc2 a@(i11,i12) b@(i21,i22) c@(i31,i32) f p1 p2 p3 (n,m) | m > 0 = 
   unsafe2PtrMapInc i11 i21 i31 f p1 p2 p3 n >> 
   (p1' `seq` p2' `seq` p3' `seq` m' `seq` unsafe2PtrMapInc2 a b c f p1' p2' p3' (n,m'))
       where p1' = advancePtr p1 i12
             p2' = advancePtr p2 i22
             p3' = advancePtr p3 i32
             m'  = m - 1
unsafe2PtrMapInc2 _ _ _ _ _ _ _ _                             | otherwise = return ()


