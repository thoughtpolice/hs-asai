-- |
-- Module      : Data.Indexed.Functor
-- Copyright   : (c) Austin Seipp 2013
-- License     : MIT
--
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Indexed @'Data.Functor.Functor'@s and @Pointed@.
--
module Data.Indexed.Functor
       ( -- * Indexed @Pointed@
         IxPointed(..) -- :: (* -> * -> * -> *) -> Constraint

         -- * Indexed @'Functor'@
       , IxFunctor(..) -- :: (* -> * -> * -> *) -> Constraint

         -- * Operators
       , (<!$>)        -- :: IxFunctor f => (a -> b) -> f s t a -> f s t b
       ) where

--------------------------------------------------------------------------------
-- Indexed pointed.

-- | Indexed @Pointed@.
class IxPointed f where
  -- | Indexed @'Prelude.return'@.
  returnI :: a -> f s s a

--------------------------------------------------------------------------------
-- Indexed functors.

-- | Indexed @'Data.Functor.Functor'@s.
class IxFunctor f where
  -- | Indexed @'Data.Functor.fmap'@.
  mapI :: (a -> b) -> f s t a -> f s t b

-- | Indexed @'Data.Functor.<$>'@.
(<!$>) :: IxFunctor f => (a -> b) -> f s t a -> f s t b
(<!$>) = mapI
{-# INLINE (<!$>) #-}
infixl 4 <!$>
