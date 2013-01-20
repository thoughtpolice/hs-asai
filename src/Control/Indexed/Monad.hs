-- |
-- Module      : Control.Indexed.Monad
-- Copyright   : (c) Oleg Kiselyov 2007-2013, (c) Austin Seipp 2012-2013
-- License     : MIT
--
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Indexed @'Monad'@s.
--
-- When using @RebindableSyntax@ with GHC, it's possible to overload
-- regular @do@-notation to use the 'IxMonad' typeclass. See the
-- section \"Using @do@-notation\" in the tutorial module
-- "Control.Delimited.Tutorial".
--
-- Currently, indexed transformers are not provided. In the future,
-- this module may be replaced by the @indexed@ or @index-core@
-- libraries which provide more general types while still having
-- @do@-notation, and providing transformers. In light of that, this
-- module will be expanded as needed.
--
module Control.Indexed.Monad
       ( -- * Indexed @'Monad'@s
         IxMonad(..) -- :: (* -> * -> * -> *) -> Constraint

         -- ** Lifting ordinary @'Monad'@s
       , MW          -- :: (* -> *) -> * -> * -> * -> *
       , lift        -- :: IxMonad m => m a -> MW m s t a
       , runI        -- :: IxMonad m => MW m s t a -> m a

         -- * Operators
       , (!>>)       -- :: IxMonad m => m s t a -> m t u b -> m s u b
       , (!>=>)      -- :: IxMonad m => (a -> m t u b) -> (b -> m s t c) -> (a -> m s u c)
       , (<=<!)      -- :: IxMonad m => (b -> m s t c) -> (a -> m t u b) -> (a -> m s u c)
       , (=<<!)      -- :: IxMonad m => (a -> m s t b) -> m t u a -> m s u b

         -- * Other functions
       , liftIxM     -- :: IxMonad m => (a -> b) -> m s t a -> m s t b
       , liftIxM2    -- :: IxMonad m => (a -> b -> c) -> m t u a -> m s t b -> m s u c
       ) where

import Data.Indexed.Functor
import Control.Indexed.Applicative

--------------------------------------------------------------------------------
-- Indexed monads.

-- | Indexed @'Monad'@s.
--
-- Regular monads can be lifted into this type class using @'lift'@.
class IxApplicative m => IxMonad m where
  -- | Indexed @'Prelude.>>='@.
  (!>>=) :: m t u a -> (a -> m s t b) -> m s u b

  -- | Indexed @'Prelude.join'@.
  joinI  :: m t u (m s t a) -> m s u a
  joinI = (!>>= id)
  {-# INLINE joinI #-}

  -- | Indexed @'Prelude.fail'@.
  failI :: String -> m s s a
  failI s = error s
  {-# INLINE failI #-}

infixl 1 !>>=

-- | This type lifts any regular @'Monad'@ into an @'IxMonad'@.
newtype MW m s t a = MW { unMW :: m a }

-- | This method 'lifts' a regular @'Monad'@ into 'MW' which
-- is an instance of 'IxMonad'.
lift :: Monad m => m a -> MW m s t a
lift = MW
{-# INLINE lift #-}

-- | This demotes a indexed monad into a regular monad. Useful
-- for when you're using e.g. @RebindableSyntax@ and want to do IO.
runI :: Monad m => MW m s t a -> m a
runI = unMW
{-# INLINE runI #-}

-- | Lifts regular instances of @'Monad'@ into @'IxFunctor'@.
instance Monad m => IxFunctor (MW m) where
  mapI f (MW x) = MW (x >>= return . f)
  {-# INLINE mapI #-}

-- | Lifts regular instances of @'Monad'@ into @'IxPointed'@.
instance Monad m => IxPointed (MW m) where
  returnI x = MW (return x)
  {-# INLINE returnI #-}

-- | Lifts regular instances of @'Monad'@ into @'IxApplicative'@.
instance Monad m => IxApplicative (MW m) where
  (MW f) <!*> (MW x) = MW (x >>= \a -> f >>= \g -> return (g a))
  {-# INLINE (<!*>) #-}

-- | Lifts regular instances of @'Monad'@ into @'IxMonad'@.
instance Monad m => IxMonad (MW m) where
  MW x !>>= f = MW (x >>= runI . f)
  {-# INLINE (!>>=) #-}

-- | Indexed @'Prelude.>>'@.
(!>>) :: IxMonad m => m s t a -> m t u b -> m s u b
(!>>) = (!*>)
{-# INLINE (!>>) #-}
infixl 1 !>>

-- | Left-to-right Kleisli composition of indexed @'Monad'@s.
(!>=>) :: IxMonad m => (a -> m t u b) -> (b -> m s t c) -> a -> m s u c
f !>=> g = \x -> f x !>>= g
{-# INLINE (!>=>) #-}

-- | Right-to-left Kleisli composition of indexed @'Monad'@s.
(<=<!) :: IxMonad m => (b -> m s t c) -> (a -> m t u b) -> a -> m s u c
(<=<!) = flip (!>=>)
{-# INLINE (<=<!) #-}

-- | Right-to-left version of @'!>>='@.
(=<<!) :: IxMonad m => (a -> m s t b) -> m t u a -> m s u b
(=<<!) = flip (!>>=)
{-# INLINE (=<<!) #-}

infixr 1 <=<!, !>=>, =<<!

-- | This is @'Control.Monad.liftM'@ for indexed @'Monad'@s.
liftIxM :: IxMonad m => (a -> b) -> m s t a -> m s t b
liftIxM f e = e !>>= returnI . f
{-# INLINE liftIxM #-}

-- | This is @'Control.Monad.liftM2'@ for indexed @'Monad'@s.
liftIxM2 :: IxMonad m => (a -> b -> c) -> m t u a -> m s t b -> m s u c
liftIxM2 f e1 e2 = e1 !>>= \x -> e2 !>>= \y -> returnI (f x y)
{-# INLINE liftIxM2 #-}
