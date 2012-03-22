{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-----------------------------------------------------------------------------
--
-- Module      :  Math.InnerProduct
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

module Numeric.Jalla.InnerProduct (
    InnerProduct(..)
) where

{-| A class that is used to define the canonical inner product on 
/CVector/ type vectors. -}
class InnerProduct o f | o -> f where
    innerProduct :: o -> o -> f




