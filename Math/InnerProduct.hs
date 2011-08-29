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

module Math.InnerProduct (
    InnerProduct(..)
) where


class InnerProduct o f | o -> f where
    innerProduct :: o -> o -> f




