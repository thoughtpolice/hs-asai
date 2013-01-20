-- |
-- Module      : Control.Indexed.Applicative
-- Copyright   : (c) Austin Seipp 2013
-- License     : MIT
--
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Indexed @'Control.Applicative.Applicative'@.
--
module Control.Indexed.Applicative
       ( -- * Indexed @'Applicative'@s
         IxApplicative(..) -- :: (* -> * -> * -> *) -> Constraint

         -- * Other functions
       , liftIxA           -- :: IxApplicative f => (a -> b) -> f t u a -> f t u b
       , liftIxA2          -- :: IxApplicative f => (a -> b -> c) -> f s t a -> f t u b -> f s u c
       ) where

import Data.Indexed.Functor

--------------------------------------------------------------------------------
-- Indexed applicatives.

-- | Indexed @'Control.Applicative.Applicative'@ class.
class (IxFunctor f, IxPointed f) => IxApplicative f where
  -- | Indexed @'Control.Applicative.<*>'@.
  (<!*>) :: f s t (a -> b) -> f t u a -> f s u b

  -- | Indexed @'Control.Applicative.*>'@.
  (!*>)  :: f s t a -> f t u b -> f s u b
  (!*>) = liftIxA2 (const id)
  {-# INLINE (!*>) #-}

  -- | Indexed @'Control.Applicative.<*'@.
  (<!*)  :: f s t a -> f t u b -> f s u a
  (<!*) = liftIxA2 const
  {-# INLINE (<!*) #-}

infixl 4 <!*>, <!*

-- | This is @'Control.Applicative.liftA'@ for indexed
-- @'Control.Applicative.Applicative'@s.
liftIxA :: IxApplicative f => (a -> b) -> f t u a -> f t u b
liftIxA f a = returnI f <!*> a
{-# INLINE liftIxA #-}

-- | This is @'Control.Applicative.liftA2'@ for indexed
-- @'Control.Applicative.Applicative'@s.
liftIxA2 :: IxApplicative f => (a -> b -> c) -> f s t a -> f t u b -> f s u c
liftIxA2 f a1 a2 = f <!$> a1 <!*> a2
{-# INLINE liftIxA2 #-}
