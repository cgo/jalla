{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
-----------------------------------------------------------------------------
--
-- Module      :  Math.IMM
-- Copyright   :  Christian Gosch
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Math.IMM (
    IMM (..)
) where

import Data.Ix

{-| Indexable objects modification monad class.
    Monads in this type class are used to modify and create indexable objects
    such as matrices or vectors. This is to provide a common interface
    for such 'modification monads'. -}
class (Ix i) => IMM m i o e | m -> o, m -> i, m -> e where
    -- These three lead to functional dependency collisions since
    -- the type of the result can not be decided by the compiler.
    --create   :: i -> m a -> o
    --modify   :: o -> m a -> o
    --getO     :: m o
    setElem  :: i -> e -> m ()
    setElems :: [(i,e)] -> m ()
    fill     :: e -> m ()
    getElem  :: i -> m e
