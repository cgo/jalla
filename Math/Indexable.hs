{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-----------------------------------------------------------------------------
--
-- Module      :  Math.Indexable
-- Copyright   :  2011 by Christian Gosch
-- License     :  BSD3
--
-- Maintainer  : Christian Gosch <werbung@goschs.de>
-- Stability   : Experimental
-- Portability : GHC only
--
-- | Indexable objects for Jalla.
-----------------------------------------------------------------------------

module Math.Indexable (
    Indexable (..)
) where

{-| Mathematical objects that can be indexed, such as matrices and vectors. -}
class Indexable o i e | o -> i, o -> e where
    {-| Get the element at a given index from an indexable object. -}
    (!) :: o   -- ^ The object to be indexed
          -> i -- ^ The index
          -> e -- ^ The element
