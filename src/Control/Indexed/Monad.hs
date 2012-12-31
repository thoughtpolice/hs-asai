-- |
-- Module      : Control.Delimited
-- Copyright   : (c) Oleg Kiselyov 2007-2012, (c) Austin Seipp 2012
-- License     : MIT
--
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Parameterized monads.
--
-- When using GHC with @RebindableSyntax@, it's possible to overload
-- regular @do@-notation to use the 'Monad'' typeclass.
--
module Control.Indexed.Monad
       ( -- * Parameterized monads
         Monad'(..)
       , (!>>=)
       , (!+>>)
       ) where

--------------------------------------------------------------------------------
-- Parameterized monads.

-- | Parameterized monads.
--
-- Regular monads are automatically lifted into this class through a
-- (hidden) newtype constructor.
class Monad' m where
  -- | Parameterized 'Prelude.return'.
  ret  :: t -> m a a t
  -- | Parameterized 'Prelude.>>='.
  bind :: m b g s -> (s -> m a b t) -> m a g t

-- | This newtype lifts any regular monad into a parameterized monad.
newtype MW m p q a = MW { unMW :: m a }

-- | This instances simply lifts regular instances of 'Monad'
-- into instances of 'Monad''.
instance Monad m => Monad' (MW m) where
  ret         x = MW (return x)
  bind (MW m) f = MW (m >>= unMW . f)

-- | Infix synonym for 'bind'.
(!>>=) :: Monad' m => m b g s -> (s -> m a b t) -> m a g t
m !>>= f = bind m f
infixl 1 !>>=

-- | Defined as:
--
-- @m1 !+>> m2 = bind m1 (const m2)@
(!+>>) :: Monad' m => m b g s -> m a b t -> m a g t
m1 !+>> m2 = bind m1 (const m2)
infixl 1 !+>>
