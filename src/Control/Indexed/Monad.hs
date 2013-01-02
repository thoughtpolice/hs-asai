-- |
-- Module      : Control.Indexed.Monad
-- Copyright   : (c) Oleg Kiselyov 2007-2012, (c) Austin Seipp 2012
-- License     : MIT
--
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Parameterized monads.
--
-- When using @RebindableSyntax@ with GHC, it's possible to overload
-- regular @do@-notation to use the 'Monad'' typeclass. See the
-- section \"Using @do@-notation\" in the tutorial module
-- "Control.Delimited.Tutorial".
--
module Control.Indexed.Monad
       ( -- * Parameterized monads
         Monad'(..) -- :: (* -> * -> * -> *) -> Constraint

         -- ** Lifting ordinary monads
       , MW         -- :: (* -> *) -> * -> * -> * -> *
       , lift       -- :: Monad m => m a -> MW m s t a
       , runI       -- :: Monad m => MW m s t a -> m a

         -- * Operators
       , (!>>=)     -- :: Monad' m => m t u a -> (a -> m s t b) -> m s u b
       , (!>>)      -- :: Monad' m => m t u a -> m s t b -> m s u b
       ) where

--------------------------------------------------------------------------------
-- Parameterized monads.

-- | Parameterized monads.
--
-- Regular monads can be lifted into this type class using 'lift'.
class Monad' m where
  -- | Parameterized 'Prelude.return'.
  ret  :: a -> m s s a
  -- | Parameterized 'Prelude.>>='.
  bind :: m t u a -> (a -> m s t b) -> m s u b

-- | This type lifts any regular monad into a parameterized monad.
newtype MW m s t a = MW { unMW :: m a }

-- | This method \'lifts\' a regular monad into a parameterized monad
-- 'MW' which is an instance of 'Monad''.
lift :: Monad m => m a -> MW m s t a
lift = MW

-- | This demotes a parameterized monad into a regular monad. Useful
-- for when you're using e.g. @RebindableSyntax@ and want to do IO.
runI :: Monad m => MW m s t a -> m a
runI = unMW

-- | This instances simply lifts regular instances of 'Monad'
-- into instances of 'Monad''.
instance Monad m => Monad' (MW m) where
  ret         x = MW (return x)
  bind (MW m) f = MW (m >>= unMW . f)

-- | Infix synonym for 'bind'.
(!>>=) :: Monad' m => m t u a -> (a -> m s t b) -> m s u b
m !>>= f = bind m f
infixl 1 !>>=

-- | 'Prelude.>>' for indexed monads.
(!>>) :: Monad' m => m t u a -> m s t b -> m s u b
m1 !>> m2 = m1 !>>= const m2
infixl 1 !>>
